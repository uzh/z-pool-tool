module Dynparam = Utils.Database.Dynparam
open Entity

let where_prefix str = Format.asprintf "WHERE %s" str

let filtered_base_condition =
  {sql|
    user_users.admin = 0
    AND user_users.confirmed = 1
    AND pool_contacts.paused = 0
    AND pool_contacts.disabled = 0
    AND NOT EXISTS
      (SELECT 1
      FROM pool_invitations
      WHERE
          pool_invitations.contact_id = pool_contacts.id
        AND
          pool_invitations.experiment_id IN (
            SELECT id FROM pool_experiments WHERE pool_experiments.uuid = UNHEX(REPLACE(?, '-', ''))
            )
      )
    AND NOT EXISTS
      (SELECT 1
      FROM pool_assignments
      WHERE
          pool_assignments.contact_id = pool_contacts.id
        AND
          pool_assignments.session_id IN (
            SELECT id FROM pool_sessions WHERE pool_sessions.experiment_uuid = UNHEX(REPLACE(?, '-', ''))
          )
      )
    |sql}
;;

let filter_to_sql template_list dyn query =
  let open Entity in
  let wrap_in_percentage operator value =
    let wrap s = CCString.concat "" [ "%"; s; "%" ] in
    let open Operator in
    match operator with
    | ContainsSome | ContainsNone | ContainsAll | Like -> wrap value
    | Less | LessEqual | Greater | GreaterEqual | Equal | NotEqual -> value
  in
  let add_value_to_params operator value dyn =
    let add c v = Dynparam.add c v dyn in
    match value with
    | Bool b -> add Caqti_type.bool b
    | Date d -> add Caqti_type.ptime d
    | Language lang -> add Caqti_type.string (Pool_common.Language.show lang)
    | Nr n -> add Caqti_type.float n
    | Option id ->
      add
        Caqti_type.string
        (id |> Custom_field.SelectOption.Id.value |> wrap_in_percentage operator)
    | Str s -> add Caqti_type.string (wrap_in_percentage operator s)
  in
  let open CCResult in
  let rec query_sql (dyn, sql) query : (Dynparam.t * 'a, 'b) result =
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
    | And queries ->
      if CCList.is_empty queries
      then Ok (dyn, sql)
      else of_list (dyn, sql) queries "AND"
    | Or queries ->
      if CCList.is_empty queries
      then Ok (dyn, sql)
      else of_list (dyn, sql) queries "OR"
    | Not f ->
      query_sql (dyn, sql) f
      >|= fun (dyn, sql) -> dyn, Format.asprintf "NOT %s" sql
    | Template id ->
      template_list
      |> CCList.find_opt (fun template -> Pool_common.Id.equal template.id id)
      |> CCOption.to_result Pool_common.Message.(NotFound Field.Template)
      >>= fun filter -> query_sql (dyn, sql) filter.query
    | Pred { Predicate.key; operator; value } ->
      let custom_field_sql =
        (* Check existence and value of rows (custom field answers) *)
        Format.asprintf
          {sql|
                SELECT (1) FROM pool_custom_field_answers
                  WHERE
                    pool_custom_field_answers.custom_field_uuid = UNHEX(REPLACE(?, '-', ''))
                  AND
                    pool_custom_field_answers.entity_uuid = user_users.uuid
                  AND
                    (%s)
              |sql}
      in
      let where_clause = Format.asprintf "%s %s ?" in
      let add_single_value dyn value =
        match key with
        | Key.Hardcoded h ->
          let dyn = add_value_to_params operator value dyn in
          let sql =
            where_clause (Key.hardcoded_to_sql h) (Operator.to_sql operator)
          in
          dyn, sql
        | Key.CustomField id ->
          let dyn =
            Dynparam.(
              dyn
              |> add Custom_field.Repo.Id.t id
              |> add_value_to_params operator value)
          in
          let sql =
            where_clause
              "COALESCE(admin_value, value)"
              (Operator.to_sql operator)
            |> custom_field_sql
          in
          dyn, sql
      in
      (match value with
       | Single value ->
         (match key with
          | Key.Hardcoded _ -> add_single_value dyn value |> CCResult.pure
          | Key.CustomField _ ->
            add_single_value dyn value
            |> fun (dyn, sql) ->
            (dyn, Format.asprintf "EXISTS (%s)" sql) |> CCResult.pure)
       | Lst [] -> Ok (dyn, sql)
       | Lst values ->
         (match key with
          | Key.Hardcoded _ ->
            Error
              Pool_common.Message.(QueryNotCompatible (Field.Value, Field.Key))
          | Key.CustomField id ->
            let dyn, subqueries =
              CCList.fold_left
                (fun (dyn, lst_sql) value ->
                  let dyn = add_value_to_params operator value dyn in
                  let new_sql =
                    where_clause
                      "COALESCE(admin_value, value)"
                      (Operator.to_sql operator)
                  in
                  dyn, lst_sql @ [ new_sql ])
                (Dynparam.(dyn |> add Custom_field.Repo.Id.t id), [])
                values
            in
            let open Operator in
            let build_query operator =
              ( dyn
              , subqueries
                |> CCList.map (Format.asprintf "(%s)")
                |> CCString.concat (Format.asprintf " %s " operator)
                |> custom_field_sql
                |> Format.asprintf "EXISTS (%s)" )
              |> CCResult.pure
            in
            (match operator with
             | ContainsAll -> build_query "AND"
             | ContainsNone -> build_query "AND"
             | ContainsSome -> build_query "OR"
             | Less
             | LessEqual
             | Greater
             | GreaterEqual
             | Equal
             | NotEqual
             | Like -> Error Pool_common.Message.(Invalid Field.Operator))))
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
