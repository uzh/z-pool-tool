module RepoEntity = Repo_entity

let find_by_contact_request =
  let open Caqti_request.Infix in
  {sql|
    SELECT
      pool_assignments.external_data_id,
      LOWER(CONCAT(
        SUBSTR(HEX(pool_experiments.uuid), 1, 8), '-',
        SUBSTR(HEX(pool_experiments.uuid), 9, 4), '-',
        SUBSTR(HEX(pool_experiments.uuid), 13, 4), '-',
        SUBSTR(HEX(pool_experiments.uuid), 17, 4), '-',
        SUBSTR(HEX(pool_experiments.uuid), 21)
      )),
      pool_experiments.title,
      LOWER(CONCAT(
        SUBSTR(HEX(pool_sessions.uuid), 1, 8), '-',
        SUBSTR(HEX(pool_sessions.uuid), 9, 4), '-',
        SUBSTR(HEX(pool_sessions.uuid), 13, 4), '-',
        SUBSTR(HEX(pool_sessions.uuid), 17, 4), '-',
        SUBSTR(HEX(pool_sessions.uuid), 21)
      )),
      pool_sessions.start,
      pool_sessions.duration
    FROM
      pool_assignments
    INNER JOIN
      pool_sessions ON pool_assignments.session_uuid = pool_sessions.uuid
    INNER JOIN
      pool_experiments ON pool_experiments.uuid = pool_sessions.experiment_uuid
    WHERE
      pool_assignments.external_data_id IS NOT NULL
    AND
      pool_assignments.contact_uuid = UNHEX(REPLACE(?, '-', ''))
    AND
      pool_assignments.marked_as_deleted = 0
    AND
      pool_assignments.participated = 1
  |sql}
  |> Caqti_type.string ->* RepoEntity.ExternalDataIdentifier.t
;;

let find_by_contact pool contact_id =
  Utils.Database.collect
    (Pool_database.Label.value pool)
    find_by_contact_request
    (Contact.Id.value contact_id)
;;
