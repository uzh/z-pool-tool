module Dynparam = Database.Dynparam

let caqti_id = Pool_common.Repo.Id.t
let caqti_tenant_id = Pool_tenant.Repo.Id.t

let sql_select_columns =
  [ Entity.Id.sql_select_fragment ~field:"pool_versions.uuid"
  ; "pool_versions.tag"
  ; "pool_versions.text"
  ; "pool_versions.published_at"
  ; "pool_versions.created_at"
  ; "pool_versions.updated_at"
  ]
;;

let insert_request =
  let open Caqti_request.Infix in
  {sql|
    INSERT INTO pool_versions (
      uuid,
      tag,
      text,
      published_at
    ) VALUES (
      UNHEX(REPLACE($1, '-', '')),
      $2,
      $3,
      $4
    )
  |sql}
  |> Repo_entity.Write.t ->. Caqti_type.unit
;;

let insert = Database.exec Database.root insert_request

let update_request =
  let open Caqti_request.Infix in
  {sql|
    UPDATE pool_versions
    SET
      tag = $2,
      text = $3,
      published_at = $4
    WHERE
      uuid = UNHEX(REPLACE($1, '-', ''))
  |sql}
  |> Repo_entity.Write.t ->. Caqti_type.unit
;;

let update = Database.exec Database.root update_request

let find_request_sql ?(count = false) where_fragment =
  let columns =
    if count then "COUNT(*)" else CCString.concat ", " sql_select_columns
  in
  Format.asprintf
    {sql|SELECT %s FROM pool_versions %s|sql}
    columns
    where_fragment
;;

let find_request =
  let open Caqti_request.Infix in
  {sql|
    WHERE pool_versions.uuid = UNHEX(REPLACE(?, '-', ''))
  |sql}
  |> find_request_sql
  |> Pool_common.Repo.Id.t ->! Repo_entity.t
;;

let find id =
  let open Utils.Lwt_result.Infix in
  Database.find_opt Database.root find_request id
  ||> CCOption.to_result Pool_message.(Error.NotFound Field.Announcement)
;;

let all ?query () =
  Query.collect_and_count
    Database.root
    query
    ~select:find_request_sql
    Repo_entity.t
;;

let all_on_tenant ?query () =
  let where = "published_at IS NOT NULL", Dynparam.empty in
  Query.collect_and_count
    Database.root
    query
    ~where
    ~select:find_request_sql
    Repo_entity.t
;;
