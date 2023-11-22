open Entity

module Name = struct
  include Name

  let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
end

let t =
  let encode m = Ok (m.id, m.name) in
  let decode (id, name) = Ok { id; name } in
  Caqti_type.(custom ~encode ~decode (t2 Pool_common.Repo.Id.t Name.t))
;;

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
      name
      FROM pool_organisational_units
  |sql}
;;

let find_request =
  let open Caqti_request.Infix in
  Format.asprintf
    {sql|
      %s
      WHERE uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    select_from
  |> Pool_common.Repo.Id.t ->! t
;;

let find pool id =
  let open Lwt.Infix in
  Utils.Database.find_opt (Pool_database.Label.value pool) find_request id
  >|= CCOption.to_result Pool_common.Message.(NotFound Field.OrganisationalUnit)
;;

let find_all_request =
  let open Caqti_request.Infix in
  select_from |> Caqti_type.unit ->* t
;;

let all pool =
  Utils.Database.collect (Pool_database.Label.value pool) find_all_request
;;

let insert_request =
  let open Caqti_request.Infix in
  {sql|
      INSERT INTO pool_organisational_units (
        uuid,
        name
      ) VALUES (
        UNHEX(REPLACE($1, '-', '')),
        $2
      )
    |sql}
  |> Caqti_type.(t2 Pool_common.Repo.Id.t Name.t ->. unit)
;;

let insert pool t =
  Utils.Database.exec
    (Pool_database.Label.value pool)
    insert_request
    (t.id, t.name)
;;

let update_request =
  let open Caqti_request.Infix in
  {sql|
    UPDATE pool_organisational_units
    SET
      name = $2
    WHERE
      uuid = UNHEX(REPLACE($1, '-', ''))
  |sql}
  |> t ->. Caqti_type.unit
;;

let update pool =
  Utils.Database.exec (Pool_database.Label.value pool) update_request
;;
