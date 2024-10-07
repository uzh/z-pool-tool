module Dynparam = Database.Dynparam

let sql_select_columns =
  [ Entity.Id.sql_select_fragment ~field:"pool_api_keys.uuid"
  ; "pool_api_keys.name"
  ; "pool_api_keys.token"
  ; "pool_api_keys.created_at"
  ; "pool_api_keys.updated_at"
  ]
;;

let insert_request =
  let open Caqti_request.Infix in
  {sql|
    INSERT INTO pool_api_keys (
      uuid,
      name,
      token
    ) VALUES (
      UNHEX(REPLACE($1, '-', '')),
      $2,
      $3
    )
  |sql}
  |> Repo_entity.Write.t ->. Caqti_type.unit
;;

let insert pool = Database.exec pool insert_request

let update_request =
  let open Caqti_request.Infix in
  {sql|
    UPDATE pool_api_keys
    SET
      name = $2
    WHERE
      uuid = UNHEX(REPLACE($1, '-', ''))
  |sql}
  |> Caqti_type.(t2 Pool_common.Repo.Id.t string) ->. Caqti_type.unit
;;

let update pool Entity.{ id; name; _ } =
  Database.exec pool update_request (id, name)
;;

let find_request_sql ?(count = false) where_fragment =
  let columns =
    if count then "COUNT(*)" else CCString.concat ", " sql_select_columns
  in
  Format.asprintf
    {sql|SELECT %s FROM pool_api_keys %s|sql}
    columns
    where_fragment
;;

let find_request =
  let open Caqti_request.Infix in
  {sql|
    WHERE pool_api_keys.uuid = UNHEX(REPLACE(?, '-', ''))
  |sql}
  |> find_request_sql
  |> Pool_common.Repo.Id.t ->! Repo_entity.t
;;

let find pool id =
  let open Utils.Lwt_result.Infix in
  Database.find_opt pool find_request id
  ||> CCOption.to_result Pool_message.(Error.NotFound Field.ApiKey)
;;

let find_by_token_request =
  let open Caqti_request.Infix in
  {sql|
    WHERE pool_api_keys.token = $1
  |sql}
  |> find_request_sql
  |> Repo_entity.Token.t ->! Repo_entity.t
;;

let find_by_token pool token =
  Database.find_opt pool find_by_token_request token
;;

let all ?query pool =
  Query.collect_and_count pool query ~select:find_request_sql Repo_entity.t
;;

let destroy_request =
  let open Caqti_request.Infix in
  {sql|
    DELETE FROM pool_api_keys
    WHERE uuid = UNHEX(REPLACE($1, '-', ''))
  |sql}
  |> Pool_common.Repo.Id.t ->. Caqti_type.unit
;;

let destroy pool Entity.{ id; _ } = Database.exec pool destroy_request id
