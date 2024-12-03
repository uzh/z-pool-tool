open Caqti_request.Infix
open Utils.Lwt_result.Infix
open Repo_entity
module Password = Repo_password_entity

let sql_select_columns =
  [ Pool_common.Id.sql_select_fragment ~field:"user_users.uuid"
  ; "user_users.email"
  ; "user_users.name"
  ; "user_users.given_name"
  ; "user_users.status"
  ; "user_users.admin"
  ; "user_users.confirmed"
  ]
;;

let sql_select_password_columns =
  [ Pool_common.Id.sql_select_fragment ~field:"user_users.uuid"
  ; "user_users.email"
  ; "user_users.password"
  ]
;;

let filter_fragment =
  {sql|
    WHERE user_users.email LIKE $1
      OR user_users.name LIKE $1
      OR user_users.given_name LIKE $1
      OR user_users.status LIKE $1
  |sql}
;;

let find_request =
  let columns = sql_select_columns |> CCString.concat ", " in
  [%string
    {sql|
      SELECT %{columns}
      FROM user_users
      WHERE user_users.uuid = %{Pool_common.Id.sql_value_fragment "?"}
    |sql}]
  |> Id.t ->? t
;;

let find_opt label = Database.find_opt label find_request

let find label id =
  find_opt label id ||> CCOption.to_result Pool_message.(Error.NotFound Field.User)
;;

let find_exn label id = find label id ||> Pool_common.Utils.get_or_failwith

let find_by_email_request =
  let columns = sql_select_columns |> CCString.concat ", " in
  [%string
    {sql|
      SELECT %{columns}
      FROM user_users
      WHERE user_users.email = ?
    |sql}]
  |> EmailAddress.t ->? t
;;

let find_by_email_opt label = Database.find_opt label find_by_email_request

let find_by_email label id =
  find_by_email_opt label id
  ||> CCOption.to_result Pool_message.(Error.NotFound Field.User)
;;

let find_by_email_exn label id =
  find_by_email label id ||> Pool_common.Utils.get_or_failwith
;;

let insert_request =
  let open Caqti_type in
  {sql|
      INSERT INTO user_users (
        uuid,
        email,
        name,
        given_name,
        password,
        status,
        admin,
        confirmed
      ) VALUES (
        UNHEX(REPLACE($1, '-', '')),
        LOWER($2),
        $3,
        $4,
        $5,
        $6,
        $7,
        $8
      )
    |sql}
  |> t2
       Id.t
       (t2
          EmailAddress.t
          (t2
             Lastname.t
             (t2 Firstname.t (t2 Password.t (t2 Status.t (t2 IsAdmin.t Confirmed.t))))))
     ->. Caqti_type.unit
;;

let insert
      label
      ( { Entity.id; email; lastname; firstname; status; admin; confirmed }
      , (password : Password.t) )
  =
  Database.exec
    label
    insert_request
    (id, (email, (lastname, (firstname, (password, (status, (admin, confirmed)))))))
;;

let update_request =
  {sql|
    UPDATE user_users
    SET
      email = LOWER($2),
      name = $3,
      given_name = $4,
      status = $5,
      admin = $6,
      confirmed = $7
    WHERE user_users.uuid = UNHEX(REPLACE($1, '-', ''))
  |sql}
  |> t ->. Caqti_type.unit
;;

let update label = Database.exec label update_request

let register_migration () =
  Database.Migration.register_migration (Repo_migration.migration ())
;;
