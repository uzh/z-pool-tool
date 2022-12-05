module Id = Pool_common.Id
module Database = Pool_database
module Dynparam = Utils.Database.Dynparam

let select_fields =
  {sql|
    SELECT
      LOWER(CONCAT(
        SUBSTR(HEX(user_users.uuid), 1, 8), '-',
        SUBSTR(HEX(user_users.uuid), 9, 4), '-',
        SUBSTR(HEX(user_users.uuid), 13, 4), '-',
        SUBSTR(HEX(user_users.uuid), 17, 4), '-',
        SUBSTR(HEX(user_users.uuid), 21)
      )),
      user_users.email,
      user_users.username,
      user_users.name,
      user_users.given_name,
      user_users.password,
      user_users.status,
      user_users.admin,
      user_users.confirmed,
      user_users.created_at,
      user_users.updated_at,
      pool_contacts.recruitment_channel,
      pool_contacts.terms_accepted_at,
      pool_contacts.language,
      pool_contacts.experiment_type_preference,
      pool_contacts.paused,
      pool_contacts.disabled,
      pool_contacts.verified,
      pool_contacts.email_verified,
      pool_contacts.num_invitations,
      pool_contacts.num_assignments,
      pool_contacts.num_show_ups,
      pool_contacts.num_participations,
      pool_contacts.firstname_version,
      pool_contacts.lastname_version,
      pool_contacts.paused_version,
      pool_contacts.language_version,
      pool_contacts.experiment_type_preference_version,
      pool_contacts.created_at,
      pool_contacts.updated_at
    FROM pool_contacts
      LEFT JOIN user_users
      ON pool_contacts.user_uuid = user_users.uuid
    |sql}
;;

let find_request_sql where_fragment =
  Format.asprintf "%s\n%s" select_fields where_fragment
;;

let find_filtered_request_sql ?limit where_fragment =
  let base = Format.asprintf "%s\n%s" select_fields where_fragment in
  match limit with
  | None -> base
  | Some limit -> Format.asprintf "%s LIMIT %i" base limit
;;

let count_filtered_request_sql where_fragment =
  let select =
    {sql|
    SELECT COUNT(*)
    FROM pool_contacts
      LEFT JOIN user_users
      ON pool_contacts.user_uuid = user_users.uuid
  |sql}
  in
  Format.asprintf "%s\n%s" select where_fragment
;;

let join_custom_field_answers =
  {sql|
  LEFT JOIN pool_custom_field_answers ON pool_custom_field_answers.entity_uuid = user_users.uuid
  |sql}
;;

let find_request =
  let open Caqti_request.Infix in
  find_request_sql
    {sql|
      WHERE user_users.uuid = UNHEX(REPLACE(?, '-', ''))
        AND user_users.admin = 0
    |sql}
  |> Caqti_type.string ->! Repo_model.t
;;

let find pool id =
  let open Utils.Lwt_result.Infix in
  Utils.Database.find_opt
    (Database.Label.value pool)
    find_request
    (Pool_common.Id.value id)
  ||> CCOption.to_result Pool_common.Message.(NotFound Field.Contact)
;;

let find_by_email_request =
  let open Caqti_request.Infix in
  find_request_sql
    {sql|
      WHERE user_users.email = ?
        AND user_users.admin = 0
    |sql}
  |> Caqti_type.string ->! Repo_model.t
;;

let find_by_email pool email =
  let open Utils.Lwt_result.Infix in
  Utils.Database.find_opt
    (Database.Label.value pool)
    find_by_email_request
    (Pool_user.EmailAddress.value email)
  ||> CCOption.to_result Pool_common.Message.(NotFound Field.Contact)
;;

let find_confirmed_request =
  let open Caqti_request.Infix in
  find_request_sql
    {sql|
      WHERE user_users.email = ?
        AND user_users.admin = 0
        AND user_users.confirmed = 1
    |sql}
  |> Caqti_type.string ->! Repo_model.t
;;

let find_confirmed pool email =
  let open Utils.Lwt_result.Infix in
  Utils.Database.find_opt
    (Database.Label.value pool)
    find_confirmed_request
    (Pool_user.EmailAddress.value email)
  ||> CCOption.to_result Pool_common.Message.(NotFound Field.Contact)
;;

let filter_to_sql template_list dyn query =
  let open Filter in
  let add_value_to_params value dyn =
    let add c v = Dynparam.add c v dyn in
    ( (match value with
       | Bool b -> add Caqti_type.bool b
       | Date d -> add Caqti_type.ptime d
       | Language lang -> add Caqti_type.string (Pool_common.Language.show lang)
       | Nr n -> add Caqti_type.float n
       | Option id -> add Custom_field.Repo.SelectOption.Id.t id
       | Str s -> add Caqti_type.string s)
    , "?" )
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
      let add_single_value dyn value =
        match key with
        | Key.Hardcoded h ->
          let dyn, param = add_value_to_params value dyn in
          let sql =
            Format.asprintf
              "%s %s %s"
              (Key.hardcoded_to_sql h)
              (Operator.to_sql operator)
              param
          in
          dyn, sql
        | Key.CustomField id ->
          let dyn, param =
            Dynparam.(
              dyn |> add Custom_field.Repo.Id.t id |> add_value_to_params value)
          in
          (* Check existence and value of rows (custom field answers) *)
          let sql =
            Format.asprintf
              {sql|
              SELECT (1) FROM pool_custom_field_answers
                WHERE
                  pool_custom_field_answers.custom_field_uuid = UNHEX(REPLACE(?, '-', ''))
                AND
                  pool_custom_field_answers.entity_uuid = user_users.uuid
                AND
                  value %s %s
            |sql}
              (Operator.to_sql operator)
              param
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
       | Lst values ->
         (match key with
          | Key.Hardcoded _ ->
            Error
              Pool_common.Message.(QueryNotCompatible (Field.Value, Field.Key))
          | Key.CustomField _ ->
            let dyn, subqueries =
              CCList.fold_left
                (fun (dyn, lst_sql) value ->
                  let dyn, new_sql = add_single_value dyn value in
                  dyn, lst_sql @ [ new_sql ])
                (dyn, [])
                values
            in
            let open Operator in
            let build_query existence operator =
              ( dyn
              , subqueries
                |> CCList.map (Format.asprintf "%s (%s)" existence)
                |> CCString.concat (Format.asprintf " %s " operator) )
              |> CCResult.pure
            in
            (match operator with
             | ContainsAll -> build_query "EXISTS" "AND"
             | ContainsNone -> build_query "NOT EXISTS" "AND"
             | ContainsSome -> build_query "EXISTS" "OR"
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

let filtered_base_condition =
  {sql|
    WHERE
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
              SELECT id FROM pool_experiments WHERE pool_experiments.uuid = UNHEX(REPLACE(?, '-', '')))
            )
        AND NOT EXISTS
        (SELECT 1
        FROM pool_assignments
        WHERE
            pool_assignments.contact_id = pool_contacts.id
          AND
            pool_assignments.session_id IN (
              SELECT id FROM pool_sessions WHERE pool_sessions.experiment_uuid = UNHEX(REPLACE(?, '-', '')))
            )
    |sql}
;;

let filtered_params ?group_by ?order_by template_list experiment_id filter =
  let open CCResult in
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

let find_filtered pool ?order_by ?limit experiment_id filter =
  let filter = filter |> CCOption.map (fun f -> f.Filter.query) in
  let open Utils.Lwt_result.Infix in
  let%lwt template_list =
    match filter with
    | None -> Lwt.return []
    | Some filter -> Filter.find_templates_of_query pool filter
  in
  filtered_params
    template_list
    ~group_by:"pool_contacts.user_uuid"
    ?order_by
    experiment_id
    filter
  |> Lwt_result.lift
  >>= fun (dyn, sql) ->
  let (Dynparam.Pack (pt, pv)) = dyn in
  let open Caqti_request.Infix in
  let request =
    sql |> find_filtered_request_sql ?limit |> pt ->* Repo_model.t
  in
  let%lwt contacts =
    Utils.Database.collect (pool |> Pool_database.Label.value) request pv
  in
  Lwt_result.return contacts
;;

let count_filtered pool experiment_id query =
  let open Utils.Lwt_result.Infix in
  let%lwt template_list =
    match query with
    | None -> Lwt.return []
    | Some query -> Filter.find_templates_of_query pool query
  in
  filtered_params template_list experiment_id query
  |> Lwt_result.lift
  >>= fun (dyn, sql) ->
  let (Dynparam.Pack (pt, pv)) = dyn in
  let open Caqti_request.Infix in
  let request = sql |> count_filtered_request_sql |> pt ->! Caqti_type.int in
  let%lwt count =
    Utils.Database.find_opt (pool |> Pool_database.Label.value) request pv
  in
  Lwt_result.return (count |> CCOption.value ~default:0)
;;

let find_multiple_request ids =
  Format.asprintf
    {sql|
    WHERE user_uuid IN ( %s )
    AND user_users.admin = 0
   |sql}
    (CCList.mapi
       (fun i _ -> Format.asprintf "UNHEX(REPLACE($%n, '-', ''))" (i + 1))
       ids
    |> CCString.concat ",")
  |> find_request_sql
;;

let find_multiple pool ids =
  let open Caqti_request.Infix in
  let dyn =
    CCList.fold_left
      (fun dyn id ->
        dyn |> Dynparam.add Caqti_type.string (id |> Pool_common.Id.value))
      Dynparam.empty
      ids
  in
  let (Dynparam.Pack (pt, pv)) = dyn in
  let request = find_multiple_request ids |> pt ->* Repo_model.t in
  Utils.Database.collect (pool |> Pool_database.Label.value) request pv
;;

let find_all_request =
  let open Caqti_request.Infix in
  find_request_sql "" |> Caqti_type.unit ->* Repo_model.t
;;

let find_all pool =
  Utils.Database.collect (Database.Label.value pool) find_all_request
;;

let insert_request =
  let open Caqti_request.Infix in
  {sql|
      INSERT INTO pool_contacts (
        user_uuid,
        recruitment_channel,
        terms_accepted_at,
        language,
        experiment_type_preference,
        paused,
        disabled,
        verified,
        email_verified,
        num_invitations,
        num_assignments,
        num_show_ups,
        num_participations,
        firstname_version,
        lastname_version,
        paused_version,
        language_version,
        experiment_type_preference_version,
        created_at,
        updated_at
      ) VALUES (
        UNHEX(REPLACE($1, '-', '')),
        $2,
        $3,
        $4,
        $5,
        $6,
        $7,
        $8,
        $9,
        $10,
        $11,
        $12,
        $13,
        $14,
        $15,
        $16,
        $17,
        $18,
        $19,
        $20
      )
    |sql}
  |> Repo_model.contact ->. Caqti_type.unit
;;

let insert pool = Utils.Database.exec (Database.Label.value pool) insert_request

let update_request =
  let open Caqti_request.Infix in
  {sql|
      UPDATE
        pool_contacts
      SET
        recruitment_channel = $2,
        terms_accepted_at = $3,
        language = $4,
        experiment_type_preference = $5,
        paused = $6,
        disabled = $7,
        verified = $8,
        email_verified = $9,
        num_invitations = $10,
        num_assignments = $11,
        num_show_ups = $12,
        num_participations = $13,
        firstname_version = $14,
        lastname_version = $15,
        paused_version = $16,
        language_version = $17,
        experiment_type_preference_version = $18
      WHERE
        user_uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
  |> Repo_model.Write.t ->. Caqti_type.unit
;;

let update pool t =
  Utils.Database.exec
    (Database.Label.value pool)
    update_request
    (Entity.Write.create t)
;;

let delete_unverified_contact_request =
  let open Caqti_request.Infix in
  {sql|
    DELETE FROM pool_contacts
    WHERE user_uuid = UNHEX(REPLACE(?, '-', '')) AND verified IS NULL
  |sql}
  |> Caqti_type.(string ->. unit)
;;

let delete_unverified_user_request =
  let open Caqti_request.Infix in
  {sql|
    DELETE FROM user_users
    WHERE uuid = UNHEX(REPLACE(?, '-', ''))
  |sql}
  |> Caqti_type.(string ->. unit)
;;

let delete_unverified_email_verifications_request =
  let open Caqti_request.Infix in
  {sql|
    DELETE FROM pool_email_verifications
    WHERE user_uuid = UNHEX(REPLACE(?, '-', '')) AND verified IS NULL
  |sql}
  |> Caqti_type.(string ->. unit)
;;

let delete_unverified pool id =
  let exec request =
    Utils.Database.exec
      (Pool_database.Label.value pool)
      request
      (Pool_common.Id.value id)
  in
  let%lwt _ = exec delete_unverified_user_request in
  let%lwt _ = exec delete_unverified_email_verifications_request in
  exec delete_unverified_contact_request
;;

let find_to_trigger_profile_update_request =
  let open Caqti_request.Infix in
  {sql|
    WHERE
      pool_contacts.paused = 0
    AND
      pool_contacts.disabled = 0
    AND
      pool_contacts.email_verified IS NOT NULL
    AND
      pool_contacts.profile_updated_at <= DATE_SUB(NOW(), INTERVAL
        (SELECT value FROM pool_system_settings WHERE settings_key = $1)
        DAY)
    AND
      pool_contacts.profile_update_triggered_at IS NULL
      OR
      (pool_contacts.profile_update_triggered_at <= DATE_SUB(NOW(), INTERVAL
        (SELECT value FROM pool_system_settings WHERE settings_key = $1)
        DAY))
    |sql}
  |> find_request_sql
  |> Caqti_type.(string) ->* Repo_model.t
;;

let find_to_trigger_profile_update pool =
  let settings_key = Settings.trigger_profile_update_after_key_yojson in
  Lwt_result.ok
  @@ Utils.Database.collect
       (Database.Label.value pool)
       find_to_trigger_profile_update_request
       (settings_key |> Yojson.Safe.to_string)
;;

let update_profile_updated_triggered_request ids =
  Format.asprintf
    {sql|
    UPDATE pool_contacts
      SET
      profile_update_triggered_at = $1
      WHERE user_uuid IN ( %s )
   |sql}
    (CCList.mapi
       (fun i _ -> Format.asprintf "UNHEX(REPLACE($%n, '-', ''))" (i + 2))
       ids
    |> CCString.concat ",")
;;

let update_profile_updated_triggered pool ids =
  let open Caqti_request.Infix in
  let dyn =
    CCList.fold_left
      (fun dyn id ->
        dyn |> Dynparam.add Caqti_type.string (id |> Pool_common.Id.value))
      (Dynparam.empty |> Dynparam.add Caqti_type.ptime (Ptime_clock.now ()))
      ids
  in
  let (Dynparam.Pack (pt, pv)) = dyn in
  let request =
    update_profile_updated_triggered_request ids
    |> (pt ->. Caqti_type.unit) ~oneshot:true
  in
  Utils.Database.exec (pool |> Pool_database.Label.value) request pv
;;
