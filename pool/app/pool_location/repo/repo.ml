include Repo_entity
module RepoFileMapping = Repo_file_mapping
module Dynparam = Utils.Database.Dynparam

let to_entity = to_entity
let of_entity = of_entity

let files_to_location pool ({ id; _ } as location) =
  let open Utils.Lwt_result.Infix in
  RepoFileMapping.find_by_location pool id ||> to_entity location
;;

module Sql = struct
  let select_sql =
    {sql|
      SELECT
        LOWER(CONCAT(
          SUBSTR(HEX(pool_locations.uuid), 1, 8), '-',
          SUBSTR(HEX(pool_locations.uuid), 9, 4), '-',
          SUBSTR(HEX(pool_locations.uuid), 13, 4), '-',
          SUBSTR(HEX(pool_locations.uuid), 17, 4), '-',
          SUBSTR(HEX(pool_locations.uuid), 21)
        )),
        pool_locations.name,
        pool_locations.description,
        pool_locations.is_virtual,
        pool_locations.institution,
        pool_locations.room,
        pool_locations.building,
        pool_locations.street,
        pool_locations.zip,
        pool_locations.city,
        pool_locations.link,
        pool_locations.status,
        pool_locations.created_at,
        pool_locations.updated_at
      FROM
        pool_locations
    |sql}
  ;;

  let search_select =
    {sql|
    SELECT
      LOWER(CONCAT(
        SUBSTR(HEX(pool_locations.uuid), 1, 8), '-',
        SUBSTR(HEX(pool_locations.uuid), 9, 4), '-',
        SUBSTR(HEX(pool_locations.uuid), 13, 4), '-',
        SUBSTR(HEX(pool_locations.uuid), 17, 4), '-',
        SUBSTR(HEX(pool_locations.uuid), 21)
      )),
      pool_locations.name
    FROM pool_locations
  |sql}
  ;;

  let find_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE
        pool_locations.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> Format.asprintf "%s\n%s" select_sql
    |> Id.t ->! t
  ;;

  let find pool id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt (Pool_database.Label.value pool) find_request id
    ||> CCOption.to_result Pool_common.Message.(NotFound Field.Location)
  ;;

  let find_all_request =
    let open Caqti_request.Infix in
    {sql|ORDER BY pool_locations.name|sql}
    |> Format.asprintf "%s\n%s" select_sql
    |> Caqti_type.unit ->* t
  ;;

  let find_all pool =
    Utils.Database.collect (Pool_database.Label.value pool) find_all_request ()
  ;;

  let insert_request =
    let open Caqti_request.Infix in
    {sql|
      INSERT INTO pool_locations (
        uuid,
        name,
        description,
        is_virtual,
        institution,
        room,
        building,
        street,
        zip,
        city,
        link,
        status,
        created_at,
        updated_at
      ) VALUES (
        UNHEX(REPLACE(?, '-', '')),
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?
      )
    |sql}
    |> t ->. Caqti_type.unit
  ;;

  let insert pool =
    Utils.Database.exec (Pool_database.Label.value pool) insert_request
  ;;

  let update_request =
    let open Caqti_request.Infix in
    {sql|
      UPDATE pool_locations
      SET
        name = $2,
        description = $3,
        is_virtual = $4,
        institution = $5,
        room = $6,
        building = $7,
        street = $8,
        zip = $9,
        city = $10,
        link = $11,
        status = $12
      WHERE
        pool_locations.uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> Caqti_type.(
         tup2
           Id.t
           (tup2
              Name.t
              (tup2
                 (option Description.t)
                 (tup2 Address.t (tup2 (option Link.t) Status.t))))
         ->. unit)
  ;;

  let update pool { Entity.id; name; description; address; link; status; _ } =
    Utils.Database.exec
      (Pool_database.Label.value pool)
      update_request
      (id, (name, (description, (address, (link, status)))))
  ;;

  let search_request ?joins ?conditions ~limit () =
    let default_contidion = "pool_locations.name LIKE ?" in
    let joined_select =
      CCOption.map_or
        ~default:search_select
        (Format.asprintf "%s %s" search_select)
        joins
    in
    let where =
      CCOption.map_or
        ~default:default_contidion
        (Format.asprintf "%s AND %s" default_contidion)
        conditions
    in
    Format.asprintf "%s WHERE %s LIMIT %i" joined_select where limit
  ;;

  let search
    ?conditions
    ?(dyn = Dynparam.empty)
    ?exclude
    ?joins
    ?(limit = 20)
    pool
    query
    =
    let open Caqti_request.Infix in
    let exclude_ids =
      Utils.Database.exclude_ids "pool_locations.uuid" Entity.Id.value
    in
    let dyn = Dynparam.(dyn |> add Caqti_type.string ("%" ^ query ^ "%")) in
    let dyn, exclude =
      exclude |> CCOption.map_or ~default:(dyn, None) (exclude_ids dyn)
    in
    let conditions =
      [ conditions; exclude ]
      |> CCList.filter_map CCFun.id
      |> function
      | [] -> None
      | conditions -> conditions |> CCString.concat " AND " |> CCOption.return
    in
    let (Dynparam.Pack (pt, pv)) = dyn in
    let request =
      search_request ?joins ?conditions ~limit ()
      |> pt ->* Caqti_type.tup2 Id.t Name.t
    in
    Utils.Database.collect (pool |> Pool_database.Label.value) request pv
  ;;

  let search_multiple_by_id_request ids =
    Format.asprintf
      {sql|
        %s
        WHERE pool_locations.uuid in ( %s )
      |sql}
      search_select
      (CCList.map (fun _ -> Format.asprintf "UNHEX(REPLACE(?, '-', ''))") ids
       |> CCString.concat ",")
  ;;

  let search_multiple_by_id pool =
    let open Caqti_request.Infix in
    function
    | [] -> Lwt.return []
    | ids ->
      let dyn =
        CCList.fold_left
          (fun dyn id ->
            dyn |> Dynparam.add Caqti_type.string (id |> Pool_common.Id.value))
          Dynparam.empty
          ids
      in
      let (Dynparam.Pack (pt, pv)) = dyn in
      let request =
        search_multiple_by_id_request ids
        |> pt ->* Caqti_type.(Repo_entity.(tup2 Id.t Name.t))
      in
      Utils.Database.collect (pool |> Pool_database.Label.value) request pv
  ;;

  let find_targets_grantable_by_admin ?exclude database_label admin query =
    let joins =
      {sql|
      LEFT JOIN guardian_actor_role_targets t ON t.target_uuid = pool_locations.uuid
        AND t.actor_uuid = UNHEX(REPLACE(?, '-', ''))
        AND t.role = ?
    |sql}
    in
    let conditions = "t.role IS NULL" in
    let dyn =
      Dynparam.(
        empty
        |> add Caqti_type.string Admin.(id admin |> Id.value)
        |> add Caqti_type.string Role.Role.(show `LocationManager))
    in
    search ~conditions ~joins ~dyn ?exclude database_label query
  ;;
end

let find pool id =
  let open Utils.Lwt_result.Infix in
  Sql.find pool id |>> files_to_location pool
;;

let find_all pool =
  let open Utils.Lwt_result.Infix in
  Sql.find_all pool >|> Lwt_list.map_s (files_to_location pool)
;;

let insert pool location files =
  let%lwt () = location |> of_entity |> Sql.insert pool in
  files |> Lwt_list.iter_s (RepoFileMapping.insert pool)
;;

let update = Sql.update
let search = Sql.search
let search_multiple_by_id = Sql.search_multiple_by_id
let find_targets_grantable_by_admin = Sql.find_targets_grantable_by_admin
