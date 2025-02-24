open CCFun.Infix
module Dynparam = Database.Dynparam
module Message = Pool_message
module Field = Pool_message.Field
open Entity

let where_prefix str = Format.asprintf "WHERE %s" str
let where_clause = Format.asprintf "%s %s ?"

(* if admin_override is set, use admin_value > value, else value *)
let coalesce_value =
  {sql| IF(pool_custom_fields.admin_override,COALESCE(admin_value,value),value) |sql}
;;

let filtered_base_condition ?(include_invited = false) =
  let base =
    {sql|
    user_users.admin = 0
    AND user_users.confirmed = 1
    AND user_users.status != ?
    AND pool_contacts.paused = 0
    AND pool_contacts.disabled = 0
    AND pool_contacts.email_verified IS NOT NULL
    |sql}
  in
  let exclude_invited =
    {sql|
    	AND (
        pool_invitations.uuid IS NULL
        OR (
          EXISTS ( SELECT 1 FROM  pool_experiment_invitation_reset WHERE experiment_uuid = pool_invitations.experiment_uuid )
          AND COALESCE(pool_invitations.resent_at, pool_invitations.created_at) <= (
            SELECT created_at
            FROM pool_experiment_invitation_reset
            WHERE experiment_uuid = pool_invitations.experiment_uuid
            ORDER BY created_at DESC
            LIMIT 1
          )
        )
      )
    |sql}
  in
  let exclude_assigned =
    {sql|
      AND NOT EXISTS (
        SELECT 1
        FROM pool_assignments
        INNER JOIN pool_sessions
          ON pool_assignments.session_uuid = pool_sessions.uuid
        WHERE pool_assignments.contact_uuid = pool_contacts.user_uuid
          AND pool_sessions.experiment_uuid = UNHEX(REPLACE(?, '-', ''))
        LIMIT 1 )
    |sql}
  in
  (function
    | MatchesFilter -> [ base ]
    | Matcher _ ->
      [ base; exclude_assigned ] @ if include_invited then [] else [ exclude_invited ])
  %> CCString.concat "\n"
;;

let custom_field_sql =
  {sql|
    SELECT (1) FROM pool_custom_field_answers
      INNER JOIN pool_custom_fields
      ON pool_custom_fields.uuid = pool_custom_field_answers.custom_field_uuid
      WHERE
        pool_custom_field_answers.custom_field_uuid = UNHEX(REPLACE(?, '-', ''))
      AND
        pool_custom_field_answers.entity_uuid = user_users.uuid
  |sql}
;;

let filter_custom_fields = Format.asprintf {sql| %s AND (%s) |sql} custom_field_sql

let add_value_to_params operator value dyn =
  let open Operator in
  let wrap_in_percentage operator value =
    let wrap s = CCString.concat "" [ "%"; s; "%" ] in
    match operator with
    | List _ | String _ -> wrap value
    | Equality _ | Size _ | Existence _ -> value
  in
  let add c v = Dynparam.add c v dyn in
  match operator with
  | Existence _ -> Error Message.(Error.Invalid Field.Predicate)
  | Equality _ | List _ | String _ | Size _ ->
    Ok
      (match value with
       | Bool b -> add Caqti_type.string (Utils.Bool.to_string b)
       | Date d -> add Pool_common.Repo.Ptime.date d
       | Language lang -> add Caqti_type.string (Pool_common.Language.show lang)
       | Nr n -> add Caqti_type.float n
       | Option id ->
         add
           Caqti_type.string
           (id |> Custom_field.SelectOption.Id.value |> wrap_in_percentage operator)
       | Str s -> add Caqti_type.string (wrap_in_percentage operator s))
;;

let add_single_value (key : Key.t) operator dyn value =
  let open CCResult in
  match key with
  | Key.Hardcoded h ->
    let* dyn = add_value_to_params operator value dyn in
    let* sql =
      Key.hardcoded_to_single_value_sql h
      >|= fun key -> where_clause key (Operator.to_sql operator)
    in
    Ok (dyn, sql)
  | Key.CustomField id ->
    let* dyn =
      Dynparam.(
        dyn |> add Custom_field.Repo.Id.t id |> add_value_to_params operator value)
    in
    let sql =
      where_clause coalesce_value (Operator.to_sql operator) |> filter_custom_fields
    in
    Ok (dyn, sql)
;;

let add_existence_condition (key : Key.t) operator dyn =
  let open CCResult in
  let open Operator.Existence in
  let format = Format.asprintf in
  match key with
  | Key.Hardcoded key ->
    let* sql =
      Key.hardcoded_to_single_value_sql key
      >|= CCFun.flip (format "%s %s") (to_sql operator)
    in
    Ok (dyn, sql)
  | Key.CustomField id ->
    let dyn = Dynparam.(dyn |> add Custom_field.Repo.Id.t id) in
    let* sql =
      let not_null_value = "pool_custom_field_answers.value IS NOT NULL" in
      let query = format "EXISTS (%s AND %s)" custom_field_sql not_null_value in
      match operator with
      | Empty -> Ok (format "NOT %s" query)
      | NotEmpty -> Ok query
    in
    Ok (dyn, sql)
;;

let add_uuid_param dyn ids =
  let open CCResult in
  CCList.fold_left
    (fun query id ->
       query
       >>= fun (dyn, params) ->
       match id with
       | Bool _ | Date _ | Language _ | Nr _ | Option _ ->
         Error Message.(Error.QueryNotCompatible (Field.Value, Field.Key))
       | Str id ->
         add_value_to_params Operator.(Equality.Equal |> equality) (Str id) dyn
         >|= fun dyn -> dyn, "UNHEX(REPLACE(?, '-', ''))" :: params)
    (Ok (dyn, []))
    ids
  >|= fun (dyn, ids) -> dyn, CCString.concat "," ids
;;

let add_list_condition subquery dyn ids =
  let open Operator in
  let compare_length (dyn, condition) =
    (dyn, Format.asprintf "(%s) %s" (subquery ~count:true) condition) |> CCResult.return
  in
  function
  | List o ->
    let open ListM in
    (match o with
     | ContainsAll ->
       (Dynparam.add Caqti_type.int (CCList.length ids) dyn, " = ? ") |> compare_length
     | ContainsNone -> Ok (dyn, Format.asprintf "NOT EXISTS (%s)" (subquery ~count:false))
     | ContainsSome -> Ok (dyn, Format.asprintf "EXISTS (%s)" (subquery ~count:false)))
  | Equality _ | String _ | Size _ | Existence _ ->
    Error Message.(Error.Invalid Field.Operator)
;;

(* The subquery returns any contacts that has been an assignment to an experiment. *)
let assignment_subquery dyn operator ids =
  let open CCResult in
  let* dyn, query_params = add_uuid_param dyn ids in
  let subquery ~count =
    let col = "DISTINCT tmp_assignments.experiment_uuid" in
    let select = if count then Format.asprintf "COUNT(%s)" col else col in
    let base =
      Format.asprintf
        {sql|
        SELECT
          %s
        FROM
          tmp_assignments
        WHERE
          tmp_assignments.contact_uuid = pool_contacts.user_uuid
        AND tmp_assignments.experiment_uuid IN (%s)
      |sql}
        select
        query_params
    in
    if count
    then Format.asprintf "%s GROUP BY tmp_assignments.contact_uuid" base
    else base
  in
  add_list_condition subquery dyn ids operator
;;

(* The subquery returns any contacts that has been invited to an experiment. *)
let invitation_subquery dyn operator ids =
  let open CCResult in
  let* dyn, query_params = add_uuid_param dyn ids in
  let subquery ~count =
    let col = "DISTINCT tmp_invitations.experiment_uuid" in
    let select = if count then Format.asprintf "COUNT(%s)" col else col in
    let base =
      Format.asprintf
        {sql|
        SELECT
          %s
        FROM
          tmp_invitations
        WHERE
          tmp_invitations.contact_uuid = pool_contacts.user_uuid
        AND tmp_invitations.experiment_uuid IN (%s)
      |sql}
        select
        query_params
    in
    if count
    then Format.asprintf "%s GROUP BY tmp_invitations.contact_uuid" base
    else base
  in
  add_list_condition subquery dyn ids operator
;;

(* The subquery does not return any contacts that have shown up at a session of the
   current experiment. It does not make a difference, if they participated. *)
let participation_subquery dyn operator ids =
  let open CCResult in
  let* dyn, query_params = add_uuid_param dyn ids in
  let subquery ~count =
    let col = "DISTINCT tmp_participations.experiment_uuid" in
    let select = if count then Format.asprintf "COUNT(%s)" col else col in
    let base =
      Format.asprintf
        {sql|
        SELECT
          %s
        FROM
          tmp_participations
        WHERE
          tmp_participations.contact_uuid = pool_contacts.user_uuid
        AND tmp_participations.experiment_uuid IN (%s)
      |sql}
        select
        query_params
    in
    if count
    then Format.asprintf "%s GROUP BY tmp_participations.contact_uuid" base
    else base
  in
  add_list_condition subquery dyn ids operator
;;

let tag_subquery dyn operator ids =
  let open CCResult in
  let* dyn, query_params = add_uuid_param dyn ids in
  let subquery ~count =
    let col = "DISTINCT pool_tags.uuid" in
    let select = if count then Format.asprintf "COUNT(%s)" col else col in
    Format.asprintf
      {sql|
        SELECT
          %s
        FROM
          pool_tagging
          INNER JOIN pool_tags ON pool_tagging.tag_uuid = pool_tags.uuid
        WHERE pool_tagging.model_uuid = pool_contacts.user_uuid
          AND pool_tags.uuid IN (%s)
        |sql}
      select
      query_params
  in
  add_list_condition subquery dyn ids operator
;;

let predicate_to_sql (dyn, sql) ({ Predicate.key; operator; value } : Predicate.t) =
  let open CCResult in
  let open Operator in
  match value with
  | NoValue ->
    (match operator with
     | Existence operator -> add_existence_condition key operator dyn
     | Equality _ | String _ | Size _ | List _ ->
       Error Message.(Error.QueryNotCompatible (Field.Value, Field.Key)))
  | Single value ->
    let add_value = add_single_value key operator in
    (match key with
     | Key.Hardcoded _ -> add_value dyn value
     | Key.CustomField _ ->
       add_value dyn value >|= fun (dyn, sql) -> dyn, Format.asprintf "EXISTS (%s)" sql)
  | Lst [] -> Ok (dyn, sql)
  | Lst values ->
    let open Key in
    (match key with
     | Hardcoded hardcoded ->
       (match hardcoded with
        | Participation -> participation_subquery dyn operator values
        | Invitation -> invitation_subquery dyn operator values
        | Assignment -> assignment_subquery dyn operator values
        | Tag -> tag_subquery dyn operator values
        | ContactLanguage
        | Firstname
        | Name
        | NumAssignments
        | NumInvitations
        | NumNoShows
        | NumParticipations
        | NumShowUps -> Error Message.(Error.QueryNotCompatible (Field.Value, Field.Key)))
     | CustomField id ->
       let* dyn, subqueries =
         CCList.fold_left
           (fun res value ->
              match res with
              | Error err -> Error err
              | Ok (dyn, lst_sql) ->
                let* dyn = add_value_to_params operator value dyn in
                let new_sql = where_clause coalesce_value (Operator.to_sql operator) in
                Ok (dyn, lst_sql @ [ new_sql ]))
           (Ok (Dynparam.(dyn |> add Custom_field.Repo.Id.t id), []))
           values
       in
       let build_query operator =
         ( dyn
         , subqueries
           |> CCList.map (Format.asprintf "(%s)")
           |> CCString.concat (Format.asprintf " %s " operator)
           |> filter_custom_fields
           |> Format.asprintf "EXISTS (%s)" )
         |> CCResult.return
       in
       (match operator with
        | List o ->
          let open ListM in
          (match o with
           | ContainsAll -> build_query "AND"
           | ContainsNone -> build_query "AND"
           | ContainsSome -> build_query "OR")
        | Equality _ | String _ | Size _ | Existence _ ->
          Error Message.(Error.Invalid Field.Operator)))
;;

let filter_to_sql template_list dyn query =
  let open Entity in
  let open CCResult in
  let rec query_sql (dyn, sql) query : (Dynparam.t * string, Pool_message.Error.t) result =
    let of_list (dyn, sql) queries operator =
      let query =
        CCList.fold_left
          (fun res query ->
             res
             >>= fun (dyn, lst_sql) ->
             query_sql (dyn, sql) query
             >|= fun (dyn, new_sql) -> dyn, lst_sql @ [ new_sql ])
          (Ok (dyn, []))
          queries
      in
      query
      >|= fun (dyn, lst_sql) ->
      ( dyn
      , lst_sql
        |> CCString.concat (Format.asprintf " %s " operator)
        |> Format.asprintf "%s (%s)" sql )
    in
    match query with
    | (And queries | Or queries) when CCList.is_empty queries -> Ok (dyn, sql)
    | And queries -> of_list (dyn, sql) queries "AND"
    | Or queries -> of_list (dyn, sql) queries "OR"
    | Not f ->
      query_sql (dyn, sql) f >|= fun (dyn, sql) -> dyn, Format.asprintf "NOT %s" sql
    | Template id ->
      template_list
      |> CCList.find_opt (fun template -> Pool_common.Id.equal template.id id)
      |> CCOption.to_result Message.(Error.NotFound Field.Template)
      >>= fun filter -> query_sql (dyn, sql) filter.query
    | Pred predicate -> predicate_to_sql (dyn, sql) predicate
  in
  query_sql (dyn, "") query
;;

let filtered_params ?include_invited ?group_by ?order_by use_case template_list filter =
  let open CCResult.Infix in
  let base_dyn =
    let open Dynparam in
    let dyn = empty |> add Caqti_type.string Pool_user.Status.(show Inactive) in
    match use_case with
    | MatchesFilter -> dyn
    | Matcher experiment_id ->
      let id = experiment_id |> Pool_common.Id.value in
      dyn |> add Caqti_type.string id
  in
  let query =
    let base_condition = filtered_base_condition ?include_invited use_case in
    match filter with
    | None -> Ok (base_dyn, base_condition)
    | Some filter ->
      filter_to_sql template_list base_dyn filter
      >|= fun (dyn, sql) -> dyn, Format.asprintf "%s\n AND %s" base_condition sql
  in
  query
  >|= fun (dyn, sql) ->
  let sql =
    match group_by with
    | None -> sql
    | Some group_by -> Format.asprintf "%s GROUP BY %s" sql group_by
  in
  let sql =
    match order_by with
    | None -> sql
    | Some order_by -> Format.asprintf "%s %s" sql order_by
  in
  dyn, sql
;;

let rec search_templates ids query =
  let search_list ids =
    CCList.fold_left (fun ids filter -> search_templates ids filter) ids
  in
  match query with
  | And lst | Or lst -> search_list ids lst
  | Not f -> search_templates ids f
  | Pred _ -> ids
  | Template id -> id :: ids
;;

let find_experiments_by_key expected_key query =
  let rec search ids query =
    let search_list ids =
      CCList.fold_left (fun ids predicate -> search ids predicate) ids
    in
    match query with
    | And lst | Or lst -> search_list ids lst
    | Not f -> search ids f
    | Template _ -> ids
    | Pred { Predicate.key; value; _ } ->
      let open Key in
      (match[@warning "-4"] key with
       | Hardcoded key ->
         if equal_hardcoded key expected_key
         then (
           match value with
           | Lst values -> values @ ids
           | _ -> ids)
         else ids
       | _ -> ids)
  in
  search [] query
  |> CCList.filter_map (fun value ->
    match value with
    | Str id -> Some id
    | Bool _ | Date _ | Language _ | Nr _ | Option _ -> None)
;;
