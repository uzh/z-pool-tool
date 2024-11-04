open Database

let caqti_id = Pool_common.Repo.Id.t
let caqti_tenant_id = Pool_tenant.Repo.Id.t

let sql_select_columns =
  [ Entity.Id.sql_select_fragment ~field:"pool_announcements.uuid"
  ; "pool_announcements.text"
  ; "pool_announcements.start_at"
  ; "pool_announcements.end_at"
  ; "pool_announcements.show_to_admins"
  ; "pool_announcements.show_to_contacts"
  ; "pool_announcements.created_at"
  ; "pool_announcements.updated_at"
  ]
;;

module TenantMapping = struct
  open Caqti_request.Infix

  let delete_existing_request tenant_ids =
    CCList.mapi
      (fun i _ -> Format.asprintf "UNHEX(REPLACE($%n, '-', ''))" (i + 2))
      tenant_ids
    |> CCString.concat ","
    |> Format.asprintf
         {sql|
            DELETE FROM pool_announcement_tenants
            WHERE pool_announcement_uuid = UNHEX(REPLACE($1, '-', ''))
            AND pool_tenant_uuid NOT IN ( %s )
          |sql}
  ;;

  let delete_all_existing_request =
    {sql|
      DELETE FROM pool_announcement_tenants
      WHERE pool_announcement_uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> caqti_id ->. Caqti_type.unit
  ;;

  let insert_request =
    let open Caqti_request.Infix in
    {sql|
      INSERT INTO pool_announcement_tenants (
        pool_announcement_uuid,
        pool_tenant_uuid
      ) VALUES (
        UNHEX(REPLACE($1, '-', '')),
        UNHEX(REPLACE($2, '-', ''))
      ) ON DUPLICATE KEY UPDATE
        updated_at = NOW()
    |sql}
    |> Caqti_type.(t2 caqti_id caqti_tenant_id ->. Caqti_type.unit)
  ;;

  let insert pool { Entity.id; _ } tenant_ids =
    match tenant_ids with
    | [] -> exec pool delete_all_existing_request id
    | tenant_ids ->
      let open Dynparam in
      let (Pack (pt, pv)) =
        let init = empty |> add caqti_id id in
        CCList.fold_left
          (fun dyn id -> add caqti_tenant_id id dyn)
          init
          tenant_ids
      in
      let delete_request =
        delete_existing_request tenant_ids |> pt ->. Caqti_type.unit
      in
      let%lwt () = exec pool delete_request pv in
      tenant_ids
      |> Lwt_list.iter_s (fun tenant_id ->
        exec pool insert_request (id, tenant_id))
  ;;

  let tenant_uuid_request =
    let open Caqti_request.Infix in
    Format.asprintf
      {sql|
        SELECT
          %s
        FROM
          pool_announcement_tenants
        WHERE
          pool_announcement_uuid = UNHEX(REPLACE($1, '-', ''))
      |sql}
      (Entity.Id.sql_select_fragment ~field:"pool_tenant_uuid")
    |> caqti_id ->* caqti_tenant_id
  ;;

  let find_tenants_by_announcement pool announcement_id =
    let open Utils.Lwt_result.Infix in
    collect pool tenant_uuid_request announcement_id
    >|> Lwt_list.map_s Pool_tenant.find
    ||> CCList.all_ok
  ;;
end

let insert_request =
  let open Caqti_request.Infix in
  {sql|
    INSERT INTO pool_announcements (
      uuid,
      text,
      start_at,
      end_at,
      show_to_admins,
      show_to_contacts
    ) VALUES (
      UNHEX(REPLACE($1, '-', '')),
      $2,
      $3,
      $4,
      $5,
      $6
    )
  |sql}
  |> Repo_entity.Write.t ->. Caqti_type.unit
;;

let insert pool (announcement, tenant_ids) =
  let%lwt () = exec pool insert_request announcement in
  TenantMapping.insert pool announcement tenant_ids
;;

let update_request =
  let open Caqti_request.Infix in
  {sql|
    UPDATE pool_announcements
    SET
      text = $2,
      start_at = $3,
      end_at = $4,
      show_to_admins = $5,
      show_to_contacts = $6
    WHERE
      uuid = UNHEX(REPLACE($1, '-', ''))
  |sql}
  |> Repo_entity.Write.t ->. Caqti_type.unit
;;

let update pool (announcement, tenant_ids) =
  let%lwt () = exec pool update_request announcement in
  TenantMapping.insert pool announcement tenant_ids
;;

let find_request_sql ?(count = false) where_fragment =
  let columns =
    if count then "COUNT(*)" else CCString.concat ", " sql_select_columns
  in
  Format.asprintf
    {sql|SELECT %s FROM pool_announcements %s|sql}
    columns
    where_fragment
;;

let find_request =
  let open Caqti_request.Infix in
  {sql|
    WHERE pool_announcements.uuid = UNHEX(REPLACE(?, '-', ''))
  |sql}
  |> find_request_sql
  |> Pool_common.Repo.Id.t ->! Repo_entity.t
;;

let find id =
  let open Utils.Lwt_result.Infix in
  find_opt Pool.Root.label find_request id
  ||> CCOption.to_result Pool_message.(Error.NotFound Field.Announcement)
;;

let all ?query pool =
  Query.collect_and_count pool query ~select:find_request_sql Repo_entity.t
;;

let find_admin id =
  let open Utils.Lwt_result.Infix in
  let pool = Pool.Root.label in
  let* announcement = find id in
  let* tenants = TenantMapping.find_tenants_by_announcement pool id in
  Lwt_result.return (announcement, tenants)
;;

let find_on_tenant_request =
  let open Caqti_request.Infix in
  {sql|
    INNER JOIN pool_announcement_tenants ON pool_announcements.uuid = pool_announcement_tenants.pool_announcement_uuid
    INNER JOIN pool_tenant ON pool_announcement_tenants.pool_tenant_uuid = pool_tenant.uuid
    WHERE
      pool_tenant.database_label = $1
    AND
      pool_announcements.uuid = UNHEX(REPLACE($2, '-', ''))
  |sql}
  |> find_request_sql
  |> Caqti_type.(t2 Repo.Label.t Pool_common.Repo.Id.t) ->! Repo_entity.t
;;

let find_of_tenant database_label id =
  let open Utils.Lwt_result.Infix in
  find_opt Pool.Root.label find_on_tenant_request (database_label, id)
  ||> CCOption.to_result Pool_message.(Error.NotFound Field.Announcement)
;;

let find_by_user_request context =
  let open Caqti_request.Infix in
  let where =
    match context with
    | `Admin -> "pool_announcements.show_to_admins = 1"
    | `Contact -> "pool_announcements.show_to_contacts = 1"
  in
  Format.asprintf
    {sql|
    INNER JOIN pool_announcement_tenants ON pool_announcements.uuid = pool_announcement_tenants.pool_announcement_uuid
    INNER JOIN pool_tenant ON pool_announcement_tenants.pool_tenant_uuid = pool_tenant.uuid
    LEFT JOIN pool_announcement_users_hide ON pool_announcements.uuid = pool_announcement_users_hide.pool_announcement_uuid
		  AND pool_announcement_users_hide.user_users_uuid = UNHEX(REPLACE($2, '-', ''))
    WHERE
      pool_tenant.database_label = $1
    AND %s
    AND(pool_announcements.start_at < NOW()
      OR pool_announcements.start_at IS NULL)
    AND(pool_announcements.end_at > NOW()
      OR pool_announcements.end_at IS NULL)
    AND pool_announcement_users_hide.user_users_uuid IS NULL
    ORDER BY pool_announcements.start_at ASC
    LIMIT 1
  |sql}
    where
  |> find_request_sql
  |> Caqti_type.(t2 Repo.Label.t Pool_common.Repo.Id.t) ->! Repo_entity.t
;;

let find_by_user database_label (context, user_id) =
  find_opt
    Pool.Root.label
    (find_by_user_request context)
    (database_label, user_id)
;;

let hide_requeset =
  let open Caqti_request.Infix in
  {sql|
    INSERT INTO pool_announcement_users_hide (
      pool_announcement_uuid,
      user_users_uuid
    ) VALUES (
      UNHEX(REPLACE($1, '-', '')),
      UNHEX(REPLACE($2, '-', ''))
    ) ON DUPLICATE KEY UPDATE
      updated_at = NOW()
  |sql}
  |> Caqti_type.(t2 caqti_id caqti_id ->. Caqti_type.unit)
;;

let hide user_id annoucement =
  exec Pool.Root.label hide_requeset (annoucement.Entity.id, user_id)
;;
