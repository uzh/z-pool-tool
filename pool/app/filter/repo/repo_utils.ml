module Dynparam = Utils.Database.Dynparam
module Field = Pool_common.Message.Field
open Entity

let where_prefix str = Format.asprintf "WHERE %s" str
let where_clause = Format.asprintf "%s %s ?"

(* if admin_override is set, use admin_value > value, else value *)
let coalesce_value =
  {sql| IF(pool_custom_fields.admin_override,COALESCE(admin_value,value),value) |sql}
;;

let filtered_base_condition =
  {sql|
    user_users.admin = 0
    AND user_users.confirmed = 1
    AND pool_contacts.paused = 0
    AND pool_contacts.disabled = 0
    AND pool_contacts.terms_accepted_at IS NOT NULL
    AND NOT EXISTS (
      SELECT
        1
      FROM
        pool_invitations
      WHERE
          pool_invitations.contact_uuid = pool_contacts.user_uuid
        AND
          pool_invitations.experiment_uuid = UNHEX(REPLACE(?, '-', ''))
      LIMIT 1 )
    AND NOT EXISTS (
      SELECT
        1
      FROM
        pool_assignments
      INNER JOIN pool_sessions
        ON pool_assignments.session_uuid = pool_sessions.uuid
      WHERE
        pool_assignments.contact_uuid = pool_contacts.user_uuid
      AND
        pool_sessions.experiment_uuid = UNHEX(REPLACE(?, '-', ''))
      LIMIT 1 )
    |sql}
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

let filter_custom_fields =
  Format.asprintf {sql| %s AND (%s) |sql} custom_field_sql
;;

let add_value_to_params operator value dyn =
  let open Operator in
  let wrap_in_percentage operator value =
    let wrap s = CCString.concat "" [ "%"; s; "%" ] in
    match operator with
    | List _ -> wrap value
    | Equality _ | String _ | Size _ | Existence _ -> value
  in
  let add c v = Dynparam.add c v dyn in
  match operator with
  | Existence _ -> Error Pool_common.Message.(Invalid Field.Predicate)
  | Equality _ | List _ | String _ | Size _ ->
    Ok
      (match value with
       | Bool b -> add Caqti_type.string (Utils.Bool.to_string b)
       | Date d -> add Caqti_type.ptime d
       | Language lang -> add Caqti_type.string (Pool_common.Language.show lang)
       | Nr n -> add Caqti_type.float n
       | Option id ->
         add
           Caqti_type.string
           (id
            |> Custom_field.SelectOption.Id.value
            |> wrap_in_percentage operator)
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
        dyn
        |> add Custom_field.Repo.Id.t id
        |> add_value_to_params operator value)
    in
    let sql =
      where_clause coalesce_value (Operator.to_sql operator)
      |> filter_custom_fields
    in
    Ok (dyn, sql)
;;

let add_existence_condition (key : Key.t) operator dyn =
  let open CCResult in
  let open Operator.Existence in
  match key with
  | Key.Hardcoded key ->
    let* sql =
      Key.hardcoded_to_single_value_sql key
      >|= fun column -> Format.asprintf "%s %s" column (to_sql operator)
    in
    Ok (dyn, sql)
  | Key.CustomField id ->
    let dyn = Dynparam.(dyn |> add Custom_field.Repo.Id.t id) in
    let* sql =
      (* TODO[timhub]: check for null values, as contacts can clear certain
         answers ? *)
      match[@warning "-4"] operator with
      | Empty -> Ok (Format.asprintf "NOT EXISTS (%s)" custom_field_sql)
      | NotEmpty -> Ok (Format.asprintf "EXISTS (%s)" custom_field_sql)
    in
    Ok (dyn, sql)
;;

(* The subquery does not return any contacts that have shown up at a session of
   the current experiment. It does not make a difference, if they
   participated. *)
let participation_subquery dyn operator ids =
  let open CCResult in
  let* dyn, query_params =
    CCList.fold_left
      (fun query id ->
        query
        >>= fun (dyn, params) ->
        match id with
        | Bool _ | Date _ | Language _ | Nr _ | Option _ ->
          Error
            Pool_common.Message.(QueryNotCompatible (Field.Value, Field.Key))
        | Str id ->
          add_value_to_params Operator.(Equality.Equal |> equality) (Str id) dyn
          >|= fun dyn -> dyn, "UNHEX(REPLACE(?, '-', ''))" :: params)
      (Ok (dyn, []))
      ids
    >|= fun (dyn, ids) -> dyn, CCString.concat "," ids
  in
  let subquery =
    Format.asprintf
      {sql|
        SELECT
          COUNT(DISTINCT pool_experiments.uuid)
        FROM
          pool_assignments
          INNER JOIN pool_sessions ON pool_sessions.uuid = pool_assignments.session_uuid
          INNER JOIN pool_experiments ON pool_sessions.experiment_uuid = pool_experiments.uuid
        WHERE
          pool_assignments.contact_uuid = pool_contacts.user_uuid
          AND pool_assignments.no_show = 0
          AND pool_assignments.canceled_at IS NULL
          AND pool_experiments.uuid IN (%s)
        GROUP BY
          pool_experiments.uuid
      |sql}
      query_params
  in
  let* condition, dyn =
    let format comparison = Format.asprintf "(%s) %s" subquery comparison in
    let open Operator in
    match operator with
    | List o ->
      let open ListM in
      (match o with
       | ContainsAll ->
         (format " = ? ", Dynparam.add Caqti_type.int (CCList.length ids) dyn)
         |> CCResult.return
       | ContainsNone -> (format " = 0 ", dyn) |> CCResult.return
       | ContainsSome -> (format " > 0 ", dyn) |> CCResult.return)
    | Equality _ | String _ | Size _ | Existence _ ->
      Error Pool_common.Message.(Invalid Field.Operator)
  in
  (dyn, Format.asprintf "(%s)" condition) |> CCResult.return
;;

let predicate_to_sql
  (dyn, sql)
  ({ Predicate.key; operator; value } : Predicate.t)
  =
  let open CCResult in
  let open Operator in
  match value with
  | NoValue ->
    (match operator with
     | Existence operator -> add_existence_condition key operator dyn
     | Equality _ | String _ | Size _ | List _ ->
       Error Pool_common.Message.(QueryNotCompatible (Field.Value, Field.Key)))
  | Single value ->
    let add_value = add_single_value key operator in
    (match key with
     | Key.Hardcoded _ -> add_value dyn value
     | Key.CustomField _ ->
       add_value dyn value
       >|= fun (dyn, sql) -> dyn, Format.asprintf "EXISTS (%s)" sql)
  | Lst [] -> Ok (dyn, sql)
  | Lst values ->
    let open Key in
    (match key with
     | Hardcoded hardcoded ->
       (match hardcoded with
        | Participation -> participation_subquery dyn operator values
        | ContactLanguage
        | Firstname
        | Name
        | NumAssignments
        | NumInvitations
        | NumNoShows
        | NumParticipations
        | NumShowUps ->
          Error
            Pool_common.Message.(QueryNotCompatible (Field.Value, Field.Key)))
     | CustomField id ->
       let* dyn, subqueries =
         CCList.fold_left
           (fun res value ->
             match res with
             | Error err -> Error err
             | Ok (dyn, lst_sql) ->
               let* dyn = add_value_to_params operator value dyn in
               let new_sql =
                 where_clause coalesce_value (Operator.to_sql operator)
               in
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
          Error Pool_common.Message.(Invalid Field.Operator)))
;;

let filter_to_sql template_list dyn query =
  let open Entity in
  let open CCResult in
  let rec query_sql (dyn, sql) query
    : (Dynparam.t * string, Pool_common.Message.error) result
    =
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
      query_sql (dyn, sql) f
      >|= fun (dyn, sql) -> dyn, Format.asprintf "NOT %s" sql
    | Template id ->
      template_list
      |> CCList.find_opt (fun template -> Pool_common.Id.equal template.id id)
      |> CCOption.to_result Pool_common.Message.(NotFound Field.Template)
      >>= fun filter -> query_sql (dyn, sql) filter.query
    | Pred predicate -> predicate_to_sql (dyn, sql) predicate
  in
  query_sql (dyn, "") query
;;

let filtered_params ?group_by ?order_by template_list experiment_id filter =
  let open CCResult.Infix in
  let id_param =
    let id = experiment_id |> Pool_common.Id.value in
    Dynparam.(empty |> add Caqti_type.string id |> add Caqti_type.string id)
  in
  let query =
    match filter with
    | None -> Ok (id_param, filtered_base_condition)
    | Some filter ->
      filter_to_sql template_list id_param filter
      >|= fun (dyn, sql) ->
      dyn, Format.asprintf "%s\n AND %s" filtered_base_condition sql
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
