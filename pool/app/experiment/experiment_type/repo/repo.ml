module Sql = struct
  module Public = struct
    module RepoEntity = Repo_entity.Public

    let select_from_experiments_sql where_fragment =
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
            description
          FROM pool_experiments
        |sql}
      in
      Format.asprintf "%s %s" select_from where_fragment
    ;;

    let find_all_request =
      let open Caqti_request.Infix in
      "" |> select_from_experiments_sql |> Caqti_type.unit ->* RepoEntity.t
    ;;

    let find_all pool =
      (* TODO [timhub]: filter experiments *)
      Utils.Database.collect (Pool_database.Label.value pool) find_all_request
    ;;

    let find_request =
      let open Caqti_request.Infix in
      {sql|
        WHERE uuid = UNHEX(REPLACE(?, '-', ''))
      |sql}
      |> select_from_experiments_sql
      |> Caqti_type.string ->! RepoEntity.t
    ;;

    let find pool id =
      let open Lwt.Infix in
      Utils.Database.find_opt
        (Pool_database.Label.value pool)
        find_request
        (id |> Pool_common.Id.value)
      >|= CCOption.to_result Pool_common.Message.(NotFound Field.Experiment)
    ;;
  end
end

let find_all_public = Sql.Public.find_all
let find_public = Sql.Public.find
