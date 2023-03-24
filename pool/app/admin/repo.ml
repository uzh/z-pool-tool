module Dynparam = Utils.Database.Dynparam

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

  let find_multiple_request ids =
    Format.asprintf
      {sql|
      WHERE uuid IN ( %s )
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
    let select_from =
      {sql|
        SELECT
          LOWER(CONCAT(
            SUBSTR(HEX(uuid), 1, 8), '-',
            SUBSTR(HEX(uuid), 9, 4), '-',
            SUBSTR(HEX(uuid), 13, 4), '-',
            SUBSTR(HEX(uuid), 17, 4), '-',
            SUBSTR(HEX(uuid), 21)
          ))
        FROM guardian_actors
      |sql}
    in
    Format.asprintf "%s %s" select_from where_fragment
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
    let open Utils.Lwt_result.Infix in
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
          , sql @ [ "roles LIKE ?" ] ))
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
        |> pt ->* Pool_common.Repo.Id.t
      in
      Utils.Database.collect (pool |> Pool_database.Label.value) request pv
      >|> find_multiple pool
  ;;
end
