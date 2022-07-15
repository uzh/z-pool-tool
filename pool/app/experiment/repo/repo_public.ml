module RepoEntity = Repo_entity

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
          pool_experiments.experiment_type
        FROM pool_experiments
      |sql}
  in
  Format.asprintf "%s %s" select_from where_fragment
;;

let contact_was_invited_join =
  {sql|
      INNER JOIN pool_invitations
      ON pool_invitations.contact_id = (SELECT id FROM pool_contacts WHERE user_uuid = UNHEX(REPLACE($1, '-', '')))
      AND pool_experiments.id = pool_invitations.experiment_id
    |sql}
;;

let condition_registration_not_disabled =
  "pool_experiments.registration_disabled = 0"
;;

let condition_allow_uninvited_signup_or_publicly_visible =
  Format.asprintf
    {sql|
      (pool_experiments.allow_uninvited_signup = 1
        OR
      pool_experiments.publicly_visible = 1)
    |sql}
;;

let find_all_public_by_contact_request =
  let contact_was_invited_fragment =
    Format.asprintf
      "%s WHERE %s"
      contact_was_invited_join
      condition_registration_not_disabled
    |> select_from_experiments_sql
  in
  let allow_uninvited_signup_fragment =
    Format.asprintf
      "WHERE %s AND %s"
      condition_registration_not_disabled
      condition_allow_uninvited_signup_or_publicly_visible
    |> select_from_experiments_sql
  in
  let open Caqti_request.Infix in
  (* TODO[timhub]: Order by what? *)
  Format.asprintf
    "%s UNION %s"
    contact_was_invited_fragment
    allow_uninvited_signup_fragment
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
  let id_fragment = "pool_experiments.uuid = UNHEX(REPLACE($2, '-', ''))" in
  let contact_was_invited_fragment =
    Format.asprintf
      "%s WHERE %s AND %s"
      contact_was_invited_join
      condition_registration_not_disabled
      id_fragment
    |> select_from_experiments_sql
  in
  let allow_uninvited_signup_fragment =
    Format.asprintf
      "WHERE %s AND %s AND %s"
      id_fragment
      condition_registration_not_disabled
      condition_allow_uninvited_signup_or_publicly_visible
    |> select_from_experiments_sql
  in
  Format.asprintf
    "%s UNION %s LIMIT 1"
    contact_was_invited_fragment
    allow_uninvited_signup_fragment
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
