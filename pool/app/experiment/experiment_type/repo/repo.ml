module Sql = struct
  module Public = struct
    module RepoEntity = Repo_entity.Public

    let contact_was_invited_join =
      {sql|
        INNER JOIN pool_invitations
        ON pool_invitations.contact_id = (SELECT id FROM pool_contacts WHERE user_uuid = UNHEX(REPLACE(?, '-', '')))
        AND pool_experiments.id = pool_invitations.experiment_id
      |sql}
    ;;

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

    let find_all_public_by_contact_request =
      let open Caqti_request.Infix in
      contact_was_invited_join
      |> select_from_experiments_sql
      |> Caqti_type.string ->* RepoEntity.t
    ;;

    let find_all_public_by_contact pool contact =
      (* TODO [timhub]: filter experiments *)
      Utils.Database.collect
        (Pool_database.Label.value pool)
        find_all_public_by_contact_request
        (Contact.id contact |> Pool_common.Id.value)
    ;;

    let find_request =
      let open Caqti_request.Infix in
      let where_fragment =
        {sql|
          WHERE pool_experiments.uuid = UNHEX(REPLACE(?, '-', ''))
        |sql}
      in
      Format.asprintf "%s %s" contact_was_invited_join where_fragment
      |> select_from_experiments_sql
      |> Caqti_type.(tup2 string string) ->! RepoEntity.t
    ;;

    let find pool id contact =
      let open Lwt.Infix in
      Utils.Database.find_opt
        (Pool_database.Label.value pool)
        find_request
        Pool_common.Id.(Contact.id contact |> value, id |> value)
      >|= CCOption.to_result Pool_common.Message.(NotFound Field.Experiment)
    ;;
  end
end

let find_all_public_by_contact = Sql.Public.find_all_public_by_contact
let find_public = Sql.Public.find
