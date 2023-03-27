module Dynparam = Utils.Database.Dynparam

module Sql = struct
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
      (find_request Pool_user.Repo.user_caqti)
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
    |> Caqti_type.unit ->* Pool_user.Repo.user_caqti
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
      let dyn =
        CCList.fold_left
          (fun dyn id ->
            dyn |> Dynparam.add Caqti_type.string (id |> Pool_common.Id.value))
          Dynparam.empty
          ids
      in
      let (Dynparam.Pack (pt, pv)) = dyn in
      let request =
        find_multiple_request ids |> pt ->* Pool_user.Repo.user_caqti
      in
      Utils.Database.collect (pool |> Pool_database.Label.value) request pv
  ;;
end

let find = Sql.find
let find_all = Sql.find_all
let find_multiple = Sql.find_multiple

module Actors = struct
  let select_from_actors where_fragment =
    let order_by = "COALESCE( user_users.given_name, user_users.name ) ASC" in
    Format.asprintf
      {sql|
        INNER JOIN guardian_actors
        ON guardian_actors.uuid = user_users.uuid
        %s
      |sql}
      where_fragment
    |> Sql.select_from_users_sql ~order_by
  ;;

  let find_all_with_role_request include_sql exclude_sql =
    let concat = CCString.concat " OR " in
    let base =
      Format.asprintf {sql|
      WHERE (%s)
     |sql} (include_sql |> concat)
    in
    (match exclude_sql with
     | None -> base
     | Some exclude_sql ->
       Format.asprintf {sql| %s AND NOT (%s) |sql} base (exclude_sql |> concat))
    |> select_from_actors
  ;;

  let find_all_with_role pool roles ~exclude =
    let add_params init =
      CCList.fold_left
        (fun (dyn, sql) role ->
          ( dyn
            |> Dynparam.add
                 Caqti_type.string
                 (role
                  |> Role.Actor.to_yojson
                  |> Yojson.Safe.to_string
                  |> fun like -> "%" ^ like ^ "%")
          , sql @ [ "guardian_actors.roles LIKE ?" ] ))
        (init, [])
    in
    if CCList.is_empty roles
    then Lwt.return []
    else
      let open Caqti_request.Infix in
      let dyn, include_sql = add_params Dynparam.empty roles in
      let dyn, exclude_sql =
        if CCList.is_empty exclude
        then dyn, None
        else add_params dyn exclude |> fun (dyn, exclude) -> dyn, Some exclude
      in
      let (Dynparam.Pack (pt, pv)) = dyn in
      let request =
        find_all_with_role_request include_sql exclude_sql
        |> pt ->* Pool_user.Repo.user_caqti
      in
      Utils.Database.collect (pool |> Pool_database.Label.value) request pv
  ;;
end
