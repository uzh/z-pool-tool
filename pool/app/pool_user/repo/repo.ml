include Repo_entity
open Caqti_request.Infix

let sql_select_columns =
  [ Pool_common.Id.sql_select_fragment ~field:"user_users.uuid"
  ; "user_users.email"
  ; "user_users.username"
  ; "user_users.name"
  ; "user_users.given_name"
  ; "user_users.password"
  ; "user_users.status"
  ; "user_users.admin"
  ; "user_users.confirmed"
  ; "user_users.created_at"
  ; "user_users.updated_at"
  ]
;;

let filter_fragment =
  {sql|
    WHERE user_users.email LIKE $1
      OR user_users.username LIKE $1
      OR user_users.name LIKE $1
      OR user_users.given_name LIKE $1
      OR user_users.status LIKE $1
  |sql}
;;

let get_request =
  let columns = sql_select_columns |> CCString.concat ", " in
  [%string
    {sql|
      SELECT %{columns}
      FROM user_users
      WHERE user_users.uuid = %{Pool_common.Id.sql_value_fragment "?"}
    |sql}]
  |> Caqti_type.string ->? Repo_entity.User.t
;;

let get label = Database.find_opt label get_request

let get_by_email_request =
  let columns = sql_select_columns |> CCString.concat ", " in
  [%string
    {sql|
      SELECT %{columns}
      FROM user_users
      WHERE user_users.email LIKE ?
    |sql}]
  |> Caqti_type.string ->? Repo_entity.User.t
;;

let get_by_email label = Database.find_opt label get_by_email_request

let insert_request =
  {sql|
      INSERT INTO user_users (
        uuid,
        email,
        username,
        name,
        given_name,
        password,
        status,
        admin,
        confirmed,
        created_at,
        updated_at
      ) VALUES (
        UNHEX(REPLACE($1, '-', '')),
        LOWER($2),
        $3,
        $4,
        $5,
        $6,
        $7,
        $8,
        $9,
        $10,
        $11
      )
    |sql}
  |> Repo_entity.User.t ->. Caqti_type.unit
;;

let insert label = Database.exec label insert_request

let update_request =
  {sql|
      UPDATE user_users
      SET
        email = LOWER($2),
        username = $3,
        name = $4,
        given_name = $5,
        password = $6,
        status = $7,
        admin = $8,
        confirmed = $9,
        created_at = $10,
        updated_at = $11
      WHERE user_users.uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
  |> Repo_entity.User.t ->. Caqti_type.unit
;;

let update label = Database.exec label update_request
let clean_request = "TRUNCATE user_users" |> Caqti_type.(unit ->. unit)
let clean label () = Database.exec label clean_request ()

let register_migration () =
  Database.Migration.register_migration (Repo_migration.migration ())
;;

let register_cleaner () =
  Sihl.Cleaner.register_cleaner (fun ?ctx () ->
    clean
      CCOption.(
        map Database.of_ctx_exn ctx
        |> get_exn_or Pool_message.(Error.(NotFound Field.Context |> show)))
      ())
;;
