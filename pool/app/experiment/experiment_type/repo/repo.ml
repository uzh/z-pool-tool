module Sql = struct
  module Public = struct
    module RepoEntity = Repo_entity.Public

    let select_from_experiments_sql where_fragment =
      let select_from =
        {sql|
          SELECT
            LOWER(CONCAT(
              SUBSTR(HEX(pool_experiments.uuid), 1, 8), '-',
              SUBSTR(HEX(pool_experiments.uuid), 9, 4), '-',
              SUBSTR(HEX(pool_experiments.uuid), 13, 4), '-',
              SUBSTR(HEX(pool_experiments.uuid), 17, 4), '-',
              SUBSTR(HEX(pool_experiments.uuid), 21)
            )),
            pool_experiments.description
          FROM pool_experiments
        |sql}
      in
      Format.asprintf "%s %s" select_from where_fragment
    ;;

    let find_all_request =
      let open Caqti_request.Infix in
      {sql|
        INNER JOIN pool_invitations
        ON pool_invitations.contact_id = (SELECT id FROM pool_contacts WHERE user_uuid = UNHEX(REPLACE(?, '-', '')))
        AND pool_experiments.id = pool_invitations.experiment_id
      |sql}
      |> select_from_experiments_sql
      |> Caqti_type.string ->* RepoEntity.t
    ;;

    let find_all pool contact =
      (* TODO [timhub]: filter experiments *)
      Utils.Database.collect
        (Pool_database.Label.value pool)
        find_all_request
        (Contact.id contact |> Pool_common.Id.value)
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
