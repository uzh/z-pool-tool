module RepoEntity = Repo_entity

let contact_was_invited_join =
  {sql|
      INNER JOIN pool_invitations
      ON pool_invitations.contact_id = (SELECT id FROM pool_contacts WHERE user_uuid = UNHEX(REPLACE(?, '-', '')))
      AND pool_experiments.id = pool_invitations.experiment_id
    |sql}
;;

let where_registration_not_disabled =
  "pool_experiments.registration_disabled = 0"
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
          pool_experiments.public_title,
          pool_experiments.description,
          pool_experiments.direct_registration_disabled,
          pool_experiments.registration_disabled
        FROM pool_experiments
      |sql}
  in
  Format.asprintf "%s %s" select_from where_fragment
;;

let find_all_public_by_contact_request =
  let open Caqti_request.Infix in
  Format.asprintf
    "%s WHERE %s"
    contact_was_invited_join
    where_registration_not_disabled
  |> select_from_experiments_sql
  |> Caqti_type.string ->* RepoEntity.Public.t
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
    Format.asprintf
      {sql|
        WHERE pool_experiments.uuid = UNHEX(REPLACE(?, '-', ''))
        AND %s
      |sql}
      where_registration_not_disabled
  in
  Format.asprintf "%s %s" contact_was_invited_join where_fragment
  |> select_from_experiments_sql
  |> Caqti_type.(tup2 string string) ->! RepoEntity.Public.t
;;

let find pool id contact =
  let open Lwt.Infix in
  Utils.Database.find_opt
    (Pool_database.Label.value pool)
    find_request
    Pool_common.Id.(Contact.id contact |> value, id |> value)
  >|= CCOption.to_result Pool_common.Message.(NotFound Field.Experiment)
;;
