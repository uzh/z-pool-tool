module Sql = struct
  let select_from_users_sql where_fragment =
    let select_from =
      {sql|
        SELECT
          LOWER(CONCAT(
            SUBSTR(HEX(uuid), 1, 8), '-',
            SUBSTR(HEX(uuid), 9, 4), '-',
            SUBSTR(HEX(uuid), 13, 4), '-',
            SUBSTR(HEX(uuid), 17, 4), '-',
            SUBSTR(HEX(uuid), 21)
          )),
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
        FROM user_users
      |sql}
    in
    Format.asprintf "%s %s" select_from where_fragment
  ;;

  let find_request caqti_type =
    let open Caqti_request.Infix in
    {sql|
      WHERE user_users.uuid = UNHEX(REPLACE(?, '-', ''))
      AND user_users.confirmed = 1
    |sql}
    |> select_from_users_sql
    |> Pool_common.Repo.Id.t ->! caqti_type
  ;;

  let find pool id =
    let open Lwt.Infix in
    Utils.Database.find_opt
      (Pool_database.Label.value pool)
      (find_request Pool_user.Repo.user_caqti)
      id
    >|= CCOption.to_result Pool_common.Message.(NotFound Field.Admin)
  ;;

  let find_all_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE
        confirmed = 1
      AND
        admin = 1
      |sql}
    |> select_from_users_sql
    |> Caqti_type.unit ->* Pool_user.Repo.user_caqti
  ;;

  let find_all pool =
    Utils.Database.collect (Pool_database.Label.value pool) find_all_request
  ;;
end

let find = Sql.find
let find_all = Sql.find_all

module Id = Pool_common.Repo.Id
