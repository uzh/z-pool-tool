include Repo_entity
module RepoFileMapping = Repo_file_mapping
module Dynparam = Utils.Database.Dynparam

let to_entity = to_entity
let of_entity = of_entity

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
    "" |> Format.asprintf "%s\n%s" select_sql |> Caqti_type.unit ->* t
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

  let search_request =
    let base =
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
        WHERE pool_locations.name LIKE $1
      |sql}
    in
    function
    | [] -> base
    | ids ->
      ids
      |> CCList.mapi (fun i _ ->
        Format.asprintf "UNHEX(REPLACE($%i, '-', ''))" (i + 2))
      |> CCString.concat ","
      |> Format.asprintf
           {sql|
              %s
              AND pool_locations.uuid NOT IN (%s)
           |sql}
           base
  ;;

  let search pool exclude query =
    let open Caqti_request.Infix in
    let dyn =
      CCList.fold_left
        (fun dyn id ->
          dyn |> Dynparam.add Caqti_type.string (id |> Entity.Id.value))
        Dynparam.(empty |> add Caqti_type.string ("%" ^ query ^ "%"))
        exclude
    in
    let (Dynparam.Pack (pt, pv)) = dyn in
    let request =
      search_request exclude
      |> pt ->* Caqti_type.(Repo_entity.(tup2 Id.t Name.t))
    in
    Utils.Database.collect (pool |> Pool_database.Label.value) request pv
  ;;

  let search_multiple_by_id_request ids =
    Format.asprintf
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
        WHERE pool_locations.uuid in ( %s )
      |sql}
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
end

let files_to_location pool ({ id; _ } as location) =
  let open Utils.Lwt_result.Infix in
  RepoFileMapping.find_by_location pool id ||> to_entity location
;;

let find pool id =
  let open Utils.Lwt_result.Infix in
  (* TODO Implement as transaction *)
  Sql.find pool id |>> files_to_location pool
;;

let find_all pool =
  let open Utils.Lwt_result.Infix in
  (* TODO Implement as transaction *)
  Sql.find_all pool >|> Lwt_list.map_s (files_to_location pool)
;;

let insert pool location files =
  let%lwt () = location |> of_entity |> Sql.insert pool in
  files |> Lwt_list.iter_s (RepoFileMapping.insert pool)
;;

let update = Sql.update
let search = Sql.search
let search_multiple_by_id = Sql.search_multiple_by_id
