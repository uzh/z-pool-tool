open Entity
module RepoPerson = Repo_person

let extract : type a. a Entity.carrier -> a Entity.t Caqti_type.t * string =
  let open Repo_person in
  let open Utils.Stringify in
  function
  | AssistantC -> assistant, person `Assistant
  | ExperimenterC -> experimenter, person `Experimenter
  | LocationManagerC -> location_manager, person `LocationManager
  | RecruiterC -> recruiter, person `Recruiter
  | OperatorC -> operator, person `Operator
  | RootC -> root, person `Root
;;

module Sql = struct
  let update_person_user_sql =
    {sql|
      UPDATE user_users
        SET
          email = $2,
          username = $3,
          name = $4,
          given_name = $5,
          password = $6,
          status = $7,
          admin = $8,
          confirmed = $9,
          created_at = $10,
          updated_at = $11
      WHERE uuid = UNHEX(REPLACE($1, '-', ''));
    |sql}
  ;;

  let update_request =
    {sql|
      UPDATE pool_person
        SET
          role = $1,
          created_at = $3,
          updated_at = $4
        WHERE sihl_user_id = UNHEX(REPLACE($2, '-', ''));
    |sql}
    |> Caqti_request.exec RepoPerson.Write.caqti
  ;;

  let update_person_user_request =
    Caqti_request.exec RepoPerson.user_caqti update_person_user_sql
  ;;

  let update (t : 'a t) =
    let person =
      Utils.Database.exec update_request (RepoPerson.Write.extract t)
    in
    let person_user =
      Utils.Database.exec update_person_user_request (Entity.user t)
    in
    Lwt_result.both person person_user
  ;;

  let select_from_persons_sql where_fragment =
    let select_from =
      {sql|
        SELECT
          pool_person.role,
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
          pool_person.created_at,
          pool_person.updated_at
        FROM pool_person
        INNER JOIN user_users ON pool_person.sihl_user_id = user_users.uuid
      |sql}
    in
    Format.asprintf "%s %s" select_from where_fragment
  ;;

  let find_all_by_role_request caqti_type =
    {sql|
      WHERE pool_person.role = ?
      AND user_users.confirmed = 1
    |sql}
    |> select_from_persons_sql
    |> Caqti_request.collect Caqti_type.string caqti_type
  ;;

  let find_all_by_role role =
    let caqti_type, role_val = extract role in
    Utils.Database.collect (find_all_by_role_request caqti_type) role_val
  ;;

  let find_request caqti_type =
    {sql|
      WHERE user_users.uuid = UNHEX(REPLACE(?, '-', ''))
      AND pool_person.role = ?
      AND user_users.confirmed = 1
    |sql}
    |> select_from_persons_sql
    |> Caqti_request.find Caqti_type.(tup2 string string) caqti_type
  ;;

  let find role (id : Pool_common.Id.t) =
    let caqti_type, role_val = extract role in
    Utils.Database.find
      (find_request caqti_type)
      (id |> Pool_common.Id.value, role_val)
  ;;

  let insert_sql =
    {sql|
      INSERT INTO pool_person (
        role,
        sihl_user_id,
        created_at,
        updated_at
      ) VALUES (
        ?,
        UNHEX(REPLACE(?, '-', '')),
        ?,
        ?
      );
    |sql}
  ;;

  let insert_request = Caqti_request.exec RepoPerson.Write.caqti insert_sql

  let insert (t : 'a t) =
    Utils.Database.exec insert_request (RepoPerson.Write.extract t)
  ;;
end

let find = Sql.find
let find_all_by_role = Sql.find_all_by_role
let insert = Sql.insert
let update = Sql.update

let set_password
    : type person. person t -> string -> string -> (unit, string) result Lwt.t
  =
 fun person password password_confirmation ->
  let open Lwt_result.Infix in
  match person with
  | Assistant { user; _ }
  | Experimenter { user; _ }
  | LocationManager { user; _ }
  | Recruiter { user; _ }
  | Operator { user; _ }
  | Root { user; _ } ->
    Service.User.set_password user ~password ~password_confirmation
    >|= CCFun.const ()
;;
