open Caqti_request.Infix
open Repo_password_entity

let find_request =
  (* NOTE: 'inactive' users shouldn't be returned *)
  [%string
    {sql|
      SELECT user_users.password
      FROM user_users
      WHERE user_users.status != 'inactive'
        AND user_users.uuid = %{Entity.Id.sql_value_fragment "?"}
    |sql}]
  |> Repo_entity.Id.t ->? t
;;

let find_opt label = Database.find_opt label find_request

let find label id =
  find_opt label id
  |> Lwt.map (CCOption.to_result Pool_message.(Error.NotFound Field.User))
;;

let find_by_email_request =
  (* NOTE: 'inactive' users shouldn't be returned *)
  {sql|
    SELECT user_users.password
    FROM user_users
    WHERE user_users.status != 'inactive'
      AND user_users.email = ?
  |sql}
  |> Repo_entity.EmailAddress.t ->? t
;;

let find_by_email_opt label = Database.find_opt label find_by_email_request

let update_request =
  [%string
    {sql|
      UPDATE user_users
      SET password = $2
      WHERE user_users.uuid = %{Entity.Id.sql_value_fragment "$1"}
    |sql}]
  |> Caqti_type.(t2 Repo_entity.Id.t t ->. unit)
;;

let update label = Database.exec label update_request
