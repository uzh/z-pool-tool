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

  let find_all_request =
    {sql|
      WHERE
        confirmed = 1
      AND
        admin = 1
      |sql}
    |> select_from_users_sql
    |> Caqti_request.collect Caqti_type.unit Pool_user.Repo.user_caqti
  ;;

  let find_all pool =
    Utils.Database.collect (Database_pool.Label.value pool) find_all_request
  ;;
end

let find_all = Sql.find_all
