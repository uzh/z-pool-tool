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

let pool_invitations_left_join =
  {sql|
      LEFT OUTER JOIN pool_invitations
      ON pool_invitations.contact_id = (SELECT id FROM pool_contacts WHERE user_uuid = UNHEX(REPLACE($1, '-', '')))
      AND pool_experiments.id = pool_invitations.experiment_id
    |sql}
;;

let condition_registration_not_disabled =
  "pool_experiments.registration_disabled = 0"
;;

let condition_allow_uninvited_signup =
  Format.asprintf
    {sql|
      pool_experiments.allow_uninvited_signup = 1
    |sql}
;;

let find_all_public_by_contact_request =
  let open Caqti_request.Infix in
  {sql| (pool_invitations.contact_id = (SELECT id FROM pool_contacts WHERE user_uuid = UNHEX(REPLACE($1, '-', '')))) |sql}
  |> Format.asprintf
       "%s WHERE %s AND (%s OR %s)"
       pool_invitations_left_join
       condition_registration_not_disabled
       condition_allow_uninvited_signup
  |> Repo.Sql.select_from_experiments_sql
  |> Pool_common.Repo.Id.t ->* RepoEntity.t
;;

let find_all_public_by_contact pool contact =
  let open Utils.Lwt_result.Infix in
  Utils.Database.collect
    (Pool_database.Label.value pool)
    find_all_public_by_contact_request
    (Contact.id contact)
  >|> Lwt_list.filter_s (fun { Entity.id; filter; _ } ->
        Contact.matches_filter pool id filter contact)
  ||> CCList.map Entity.to_public
;;

let find_request =
  let open Caqti_request.Infix in
  let id_fragment = "pool_experiments.uuid = UNHEX(REPLACE($2, '-', ''))" in
  {sql| (pool_invitations.contact_id = (SELECT id FROM pool_contacts WHERE user_uuid = UNHEX(REPLACE($1, '-', '')))) |sql}
  |> Format.asprintf
       "%s WHERE %s AND %s AND (%s OR %s)"
       pool_invitations_left_join
       id_fragment
       condition_registration_not_disabled
       condition_allow_uninvited_signup
  |> select_from_experiments_sql
  |> Caqti_type.(tup2 string string) ->! RepoEntity.Public.t
;;

let find pool id contact =
  let open Utils.Lwt_result.Infix in
  Utils.Database.find_opt
    (Pool_database.Label.value pool)
    find_request
    Pool_common.Id.(Contact.id contact |> value, id |> value)
  ||> CCOption.to_result Pool_common.Message.(NotFound Field.Experiment)
;;
