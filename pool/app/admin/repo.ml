open CCFun.Infix
open Caqti_request.Infix
open Repo_entity
module Dynparam = Database.Dynparam

let sql_select_columns =
  Pool_user.Repo.sql_select_columns
  @ [ "pool_admins.email_verified"; "pool_admins.import_pending" ]
;;

let user_join =
  {sql|
    LEFT JOIN user_users
      ON pool_admins.user_uuid = user_users.uuid
  |sql}
;;

let insert_request =
  {sql|
    INSERT INTO pool_admins (
      user_uuid,
      email_verified,
      import_pending
    ) VALUES (
      UNHEX(REPLACE($1, '-', '')),
      $2,
      $3
    )
  |sql}
  |> Write.t ->. Caqti_type.unit
;;

let insert pool = Write.of_entity %> Database.exec pool insert_request

let find_request_sql ?(joins = []) ?(count = false) =
  let columns =
    if count
    then "COUNT(DISTINCT pool_admins.user_uuid)"
    else sql_select_columns |> CCString.concat ", "
  in
  let joins = user_join :: joins |> CCString.concat "\n" in
  Format.asprintf {sql|SELECT DISTINCT %s FROM pool_admins %s %s|sql} columns joins
;;

let find_request =
  {sql| WHERE user_users.uuid = UNHEX(REPLACE(?, '-', '')) |sql}
  |> find_request_sql
  |> Id.t ->! t
;;

let find pool id =
  let open Lwt.Infix in
  Database.find_opt pool find_request id
  >|= CCOption.to_result Pool_message.(Error.NotFound Field.Admin)
;;

let find_by_email_request =
  {sql| WHERE user_users.email = ? |sql}
  |> find_request_sql
  |> Pool_user.Repo.EmailAddress.t ->! t
;;

let find_by_email pool email =
  let open Lwt.Infix in
  Database.find_opt pool find_by_email_request email
  >|= CCOption.to_result Pool_message.(Error.NotFound Field.Admin)
;;

let find_all_request =
  {sql| WHERE user_users.admin = 1 |sql} |> find_request_sql |> Caqti_type.unit ->* t
;;

(* TODO: One is not required, find_by / list_by_user *)
let find_by ?query pool =
  Query.collect_and_count pool query ~select:(find_request_sql ?joins:None) t
;;

let all ?query pool =
  Query.collect_and_count pool query ~select:(find_request_sql ?joins:None) t
;;

let list_by_user ?query pool actor =
  let open CCFun.Infix in
  let dyn, sql, joins =
    Guard.Persistence.with_user_permission actor "pool_admins.user_uuid" `Admin
  in
  let select ?count =
    find_request_sql ?count ~joins:[ joins ] %> Format.asprintf "%s %s" sql
  in
  Query.collect_and_count pool query ~select ~dyn Repo_entity.t
;;

let find_multiple_request ids =
  Format.asprintf
    {sql| WHERE user_users.uuid IN ( %s ) |sql}
    (CCList.mapi (fun i _ -> Format.asprintf "UNHEX(REPLACE($%n, '-', ''))" (i + 1)) ids
     |> CCString.concat ",")
  |> find_request_sql
;;

let find_multiple pool ids =
  if CCList.is_empty ids
  then Lwt.return []
  else (
    let (Dynparam.Pack (pt, pv)) =
      CCList.fold_left
        (fun dyn id -> dyn |> Dynparam.add Pool_user.Repo.Id.t id)
        Dynparam.empty
        ids
    in
    let request = find_multiple_request ids |> pt ->* t in
    Database.collect pool request pv)
;;

let query_by_role ?query ?exclude pool (role, target_uuid) =
  let open Dynparam in
  let joins =
    {sql| 
      LEFT JOIN guardian_actor_roles 
        ON guardian_actor_roles.actor_uuid = pool_admins.user_uuid 
        AND guardian_actor_roles.mark_as_deleted IS NULL
      LEFT JOIN guardian_actor_role_targets 
        ON guardian_actor_role_targets.actor_uuid = pool_admins.user_uuid 
        AND guardian_actor_role_targets.mark_as_deleted IS NULL
    |sql}
  in
  let where, dyn =
    let dyn = empty |> add Caqti_type.string (Role.Role.show role) in
    match target_uuid with
    | None -> "guardian_actor_roles.role = ?", dyn
    | Some target_uuid ->
      ( {sql| guardian_actor_role_targets.role = ? AND guardian_actor_role_targets.target_uuid = UNHEX(REPLACE(?, '-', '')) |sql}
      , dyn |> add Caqti_type.string (Guard.Uuid.Target.to_string target_uuid) )
  in
  let exclude, dyn =
    let not_exists =
      Format.asprintf
        {sql| NOT EXISTS (SELECT 1 FROM %s t WHERE t.actor_uuid = pool_admins.user_uuid AND t.mark_as_deleted IS NULL AND %s) |sql}
    in
    match exclude with
    | None -> [], dyn
    | Some exclude ->
      exclude
      |> CCList.fold_left
           (fun (sql, dyn) (role, target_uuid) ->
              let dyn = dyn |> add Caqti_type.string (Role.Role.show role) in
              match target_uuid with
              | None -> sql @ [ not_exists "guardian_actor_roles" "t.role = ?" ], dyn
              | Some target_uuid ->
                let dyn =
                  add Caqti_type.string (Guard.Uuid.Target.to_string target_uuid) dyn
                in
                ( sql
                  @ [ not_exists
                        "guardian_actor_role_targets"
                        "t.role = ? AND t.target_uuid = UNHEX(REPLACE(?, '-', ''))"
                    ]
                , dyn ))
           ([], dyn)
  in
  let where = where :: exclude |> CCString.concat " AND " in
  Query.collect_and_count
    pool
    query
    ~where
    ~dyn
    ~select:(find_request_sql ~joins:[ joins ])
    t
;;

let update_request =
  {sql|
    UPDATE
      pool_admins
    SET
      email_verified = $2,
      import_pending = $3
    WHERE
      user_uuid = UNHEX(REPLACE($1, '-', ''))
  |sql}
  |> Write.t ->. Caqti_type.unit
;;

let update pool = Write.of_entity %> Database.exec pool update_request

let update_sign_in_count_request =
  {sql|
    UPDATE
      pool_admins
    SET
      sign_in_count = sign_in_count + 1,
      last_sign_in_at = NOW()
    WHERE
      user_uuid = UNHEX(REPLACE($1, '-', ''))
  |sql}
  |> Id.t ->. Caqti_type.unit
;;

let update_sign_in_count pool =
  Entity.id %> Database.exec pool update_sign_in_count_request
;;

let promote_contact_insert_contact_to_promoted_request =
  {sql|
    INSERT INTO pool_contacts_promoted (
      user_uuid,
      terms_accepted_at,
      language,
      cell_phone,
      sms_deactivated,
      experiment_type_preference,
      paused,
      disabled,
      verified,
      email_verified,
      secondary_email,
      secondary_email_verified,
      num_invitations,
      num_assignments,
      num_show_ups,
      num_no_shows,
      num_participations,
      firstname_version,
      lastname_version,
      paused_version,
      language_version,
      experiment_type_preference_version,
      sign_in_count,
      last_sign_in_at,
      admin_notes,
      profile_updated_at,
      registration_attempt_notification_sent_at,
      profile_update_triggered_at,
      import_pending,
      smtp_bounces_count,
      created_at,
      updated_at
    )
    SELECT
      user_uuid,
      terms_accepted_at,
      language,
      cell_phone,
      sms_deactivated,
      experiment_type_preference,
      paused,
      disabled,
      verified,
      email_verified,
      secondary_email,
      secondary_email_verified,
      num_invitations,
      num_assignments,
      num_show_ups,
      num_no_shows,
      num_participations,
      firstname_version,
      lastname_version,
      paused_version,
      language_version,
      experiment_type_preference_version,
      sign_in_count,
      last_sign_in_at,
      admin_notes,
      profile_updated_at,
      registration_attempt_notification_sent_at,
      profile_update_triggered_at,
      import_pending,
      smtp_bounces_count,
      created_at,
      updated_at
    FROM pool_contacts
    WHERE user_uuid = UNHEX(REPLACE($1, '-', ''))
  |sql}
  |> Pool_user.Repo.Id.t ->. Caqti_type.unit
;;

let promote_contact_insert_admin_request =
  {sql|
    INSERT INTO pool_admins (user_uuid, email_verified, sign_in_count, last_sign_in_at)
    SELECT user_uuid, email_verified, sign_in_count, last_sign_in_at
    FROM pool_contacts
    WHERE user_uuid = UNHEX(REPLACE($1, '-', ''))
  |sql}
  |> Pool_user.Repo.Id.t ->. Caqti_type.unit
;;

let promote_contact_set_admin_request =
  {sql|
    UPDATE user_users
    SET admin = 1
    WHERE uuid = UNHEX(REPLACE($1, '-', ''))
  |sql}
  |> Pool_user.Repo.Id.t ->. Caqti_type.unit
;;

let promote_contact_delete_contact_request =
  {sql|
    DELETE FROM pool_contacts
    WHERE user_uuid = UNHEX(REPLACE($1, '-', ''))
  |sql}
  |> Pool_user.Repo.Id.t ->. Caqti_type.unit
;;

let promote_contact pool id =
  [ promote_contact_insert_contact_to_promoted_request, id
  ; promote_contact_insert_admin_request, id
  ; promote_contact_set_admin_request, id
  ; promote_contact_delete_contact_request, id
  ]
  |> CCList.map (fun (request, input) connection ->
    let (module Connection : Caqti_lwt.CONNECTION) = connection in
    Connection.exec request input)
  |> Database.transaction_iter pool
;;

let search_by_name_and_email_request ?conditions =
  let default_contidion =
    {sql|
      (user_users.email LIKE ? OR CONCAT(user_users.given_name, ' ', user_users.name, ' ', user_users.given_name) LIKE ? )
    |sql}
  in
  let where =
    CCOption.map_or
      ~default:default_contidion
      (Format.asprintf "%s AND %s" default_contidion)
      conditions
  in
  Format.asprintf
    "SELECT %s FROM pool_admins %s WHERE %s LIMIT %i"
    (sql_select_columns |> CCString.concat ", ")
    user_join
    where
;;

let search_by_name_and_email ?(dyn = Dynparam.empty) ?exclude ?(limit = 20) pool query =
  let open Caqti_request.Infix in
  let exclude_ids = Database.exclude_ids "pool_admins.uuid" Id.value in
  let add_query = Dynparam.add Caqti_type.string ("%" ^ query ^ "%") in
  let dyn = dyn |> add_query |> add_query in
  let dyn, exclude = exclude |> CCOption.map_or ~default:(dyn, None) (exclude_ids dyn) in
  let conditions =
    [ exclude ]
    |> CCList.filter_map CCFun.id
    |> function
    | [] -> None
    | conditions -> conditions |> CCString.concat " AND " |> CCOption.return
  in
  let (Dynparam.Pack (pt, pv)) = dyn in
  let request = search_by_name_and_email_request ?conditions limit |> pt ->* t in
  Database.collect pool request pv
;;
