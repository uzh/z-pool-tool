module RepoEntity = Repo_entity

module Sql = struct
  let select_sql =
    {sql|
      SELECT
        LOWER(CONCAT(
          SUBSTR(HEX(pool_assignments.uuid), 1, 8), '-',
          SUBSTR(HEX(pool_assignments.uuid), 9, 4), '-',
          SUBSTR(HEX(pool_assignments.uuid), 13, 4), '-',
          SUBSTR(HEX(pool_assignments.uuid), 17, 4), '-',
          SUBSTR(HEX(pool_assignments.uuid), 21)
        )),
        pool_assignments.canceled_at,
        LOWER(CONCAT(
          SUBSTR(HEX(pool_sessions.uuid), 1, 8), '-',
          SUBSTR(HEX(pool_sessions.uuid), 9, 4), '-',
          SUBSTR(HEX(pool_sessions.uuid), 13, 4), '-',
          SUBSTR(HEX(pool_sessions.uuid), 17, 4), '-',
          SUBSTR(HEX(pool_sessions.uuid), 21)
        )),
        pool_sessions.start,
        pool_sessions.duration,
        pool_sessions.description,
        pool_sessions.canceled_at
      FROM pool_assignments
      INNER JOIN pool_sessions
        ON pool_assignments.session_id = pool_sessions.id
    |sql}
  ;;

  let find_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE
        pool_sessions.experiment_uuid = UNHEX(REPLACE(?, '-', ''))
      AND
        pool_assignments.contact_id = (SELECT id FROM pool_contacts WHERE user_uuid = UNHEX(REPLACE(?, '-', '')))
    |sql}
    |> Format.asprintf "%s\n%s" select_sql
    |> Caqti_type.(tup2 string string) ->! RepoEntity.public_assignment
  ;;

  let find_opt_by_experiment pool contact experiment =
    Utils.Database.find_opt
      (Pool_database.Label.value pool)
      find_request
      Pool_common.(
        Id.value experiment.Experiment_type.id, Contact.id contact |> Id.value)
  ;;
end

let find_opt_by_experiment = Sql.find_opt_by_experiment
