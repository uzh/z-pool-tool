open Caqti_request.Infix
open Utils.Lwt_result.Infix
open Repo_entity
module Password = Repo_password_entity

let make_sql_select_columns ~tablename =
  let with_tablemame = Format.asprintf "%s.%s" tablename in
  [ Pool_common.Id.sql_select_fragment ~field:(with_tablemame "uuid") ]
  @ ([ "email"; "name"; "given_name"; "status"; "admin"; "confirmed" ]
     |> CCList.map with_tablemame)
;;

let sql_select_columns = make_sql_select_columns ~tablename:"user_users"

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

let increment_smtp_bounce_request =
  (* the [user_users.admin] predicate in each join updates either the contact
     or the admin record; a contact whose new count reaches 5 is also paused
     and its paused_version incremented. *)
  {sql|
    UPDATE user_users
    LEFT JOIN pool_contacts
      ON pool_contacts.user_uuid = user_users.uuid
      AND user_users.admin = 0
    LEFT JOIN pool_admins
      ON pool_admins.user_uuid = user_users.uuid
      AND user_users.admin = 1
    SET
      pool_contacts.smtp_bounces_count =
        LEAST(pool_contacts.smtp_bounces_count + 1, 32767),
      pool_contacts.paused =
        CASE WHEN pool_contacts.smtp_bounces_count + 1 >= 5 THEN 1
             ELSE pool_contacts.paused
        END,
      pool_contacts.paused_version =
        CASE WHEN pool_contacts.smtp_bounces_count + 1 >= 5
             THEN pool_contacts.paused_version + 1
             ELSE pool_contacts.paused_version
        END,
      pool_admins.smtp_bounces_count =
        LEAST(pool_admins.smtp_bounces_count + 1, 32767)
    WHERE user_users.email = ?
  |sql}
  |> EmailAddress.t ->. Caqti_type.unit
;;

let increment_smtp_bounce label = Database.exec label increment_smtp_bounce_request

let reset_smtp_bounce_request =
  {sql|
    UPDATE user_users
    LEFT JOIN pool_contacts
      ON pool_contacts.user_uuid = user_users.uuid
      AND user_users.admin = 0
    LEFT JOIN pool_admins
      ON pool_admins.user_uuid = user_users.uuid
      AND user_users.admin = 1
    SET
      pool_contacts.smtp_bounces_count = 0,
      pool_admins.smtp_bounces_count = 0
    WHERE user_users.email = ?
  |sql}
  |> EmailAddress.t ->. Caqti_type.unit
;;

let reset_smtp_bounce label = Database.exec label reset_smtp_bounce_request

let register_migration () =
  Database.Migration.register_migration (Repo_migration.migration ())
;;
