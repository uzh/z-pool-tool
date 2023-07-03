module Dynparam = Utils.Database.Dynparam

module Sql = struct
  let insert_request =
    let open Caqti_request.Infix in
    {sql|
      INSERT INTO pool_admins (
        user_uuid,
        created_at,
        updated_at
      ) VALUES (
        UNHEX(REPLACE($1, '-', '')),
        NOW(),
        NOW()
      )
    |sql}
    |> Pool_common.Repo.Id.t ->. Caqti_type.unit
  ;;

  let insert pool t =
    t
    |> Entity.id
    |> Utils.Database.exec (Pool_database.Label.value pool) insert_request
  ;;

  let select_from_users_sql ?order_by where_fragment =
    let select_from =
      {sql|
        SELECT
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
          user_users.updated_at
        FROM user_users
        INNER JOIN pool_admins
        ON pool_admins.user_uuid = user_users.uuid
      |sql}
    in
    let query = Format.asprintf "%s %s" select_from where_fragment in
    match order_by with
    | Some order_by -> Format.asprintf "%s ORDER BY %s" query order_by
    | None -> query
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
      (find_request Repo_entity.t)
      id
    >|= CCOption.to_result Pool_common.Message.(NotFound Field.Admin)
  ;;

  let find_all_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE
      user_users.confirmed = 1
      AND
      user_users.admin = 1
      |sql}
    |> select_from_users_sql
    |> Caqti_type.unit ->* Repo_entity.t
  ;;

  let find_all pool =
    Utils.Database.collect (Pool_database.Label.value pool) find_all_request
  ;;

  let find_multiple_request ids =
    Format.asprintf
      {sql|
        WHERE user_users.uuid IN ( %s )
      |sql}
      (CCList.mapi
         (fun i _ -> Format.asprintf "UNHEX(REPLACE($%n, '-', ''))" (i + 1))
         ids
       |> CCString.concat ",")
    |> select_from_users_sql
  ;;

  let find_multiple pool ids =
    if CCList.is_empty ids
    then Lwt.return []
    else
      let open Caqti_request.Infix in
      let (Dynparam.Pack (pt, pv)) =
        CCList.fold_left
          (fun dyn id ->
            dyn |> Dynparam.add Caqti_type.string (id |> Pool_common.Id.value))
          Dynparam.empty
          ids
      in
      let request = find_multiple_request ids |> pt ->* Repo_entity.t in
      Utils.Database.collect (pool |> Pool_database.Label.value) request pv
  ;;

  let update_sign_in_count_request =
    let open Caqti_request.Infix in
    {sql|
      UPDATE
        pool_admins
      SET
        sign_in_count = sign_in_count + 1,
        last_sign_in_at = NOW()
      WHERE
        user_uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> Caqti_type.(string ->. unit)
  ;;

  let update_sign_in_count pool t =
    Utils.Database.exec
      (Pool_database.Label.value pool)
      update_sign_in_count_request
      Entity.(id t |> Id.value)
  ;;
end

let insert = Sql.insert
let find = Sql.find
let find_all = Sql.find_all
let find_multiple = Sql.find_multiple

(* TODO: Call this on admin login. Depending on
   https://github.com/uzh/pool/pull/147 *)
let update_sign_in_count = Sql.update_sign_in_count
