module RepoEntity = Repo_entity

let select_from_experiments_sql ?(distinct = false) where_fragment =
  let select_from =
    Format.asprintf
      {sql|
        SELECT %s
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
      (if distinct then "DISTINCT" else "")
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
  let not_assigned =
    {sql|
    NOT EXISTS (
        SELECT
          1 FROM pool_assignments
        WHERE
          pool_assignments.marked_as_deleted = 0
        AND
          pool_assignments.contact_uuid = UNHEX(REPLACE($1, '-', ''))
        AND pool_assignments.session_uuid IN(
          SELECT
            uuid FROM pool_sessions
          WHERE
            pool_sessions.experiment_uuid = pool_experiments.uuid))
      |sql}
  in
  let not_on_waitinglist =
    {sql|
    NOT EXISTS (
      SELECT
        1 FROM pool_waiting_list
      WHERE
        pool_waiting_list.contact_id = (
          SELECT
            id FROM pool_contacts
          WHERE
            user_uuid = UNHEX(REPLACE($1, '-', '')))
          AND pool_waiting_list.experiment_id = pool_experiments.id)
      |sql}
  in
  let is_invited =
    {sql| (pool_invitations.contact_id = (SELECT id FROM pool_contacts WHERE user_uuid = UNHEX(REPLACE($1, '-', '')))) |sql}
  in
  Format.asprintf
    "%s WHERE %s AND %s AND %s AND (%s OR %s)"
    pool_invitations_left_join
    not_assigned
    not_on_waitinglist
    condition_registration_not_disabled
    condition_allow_uninvited_signup
    is_invited
  |> Repo.Sql.select_from_experiments_sql
  |> Pool_common.Repo.Id.t ->* RepoEntity.t
;;

let find_all_public_by_contact pool contact =
  let open Utils.Lwt_result.Infix in
  Utils.Database.collect
    (Pool_database.Label.value pool)
    find_all_public_by_contact_request
    (Contact.id contact)
  >|> Lwt_list.filter_s (fun { Entity.id; filter; allow_uninvited_signup; _ } ->
        if allow_uninvited_signup
        then Filter.contact_matches_filter pool id filter contact
        else Lwt.return_true)
  ||> CCList.map Entity.to_public
;;

let find_where_contact_is_on_waitinglist_request =
  let open Caqti_request.Infix in
  let join =
    {sql|
    INNER JOIN pool_waiting_list
      ON pool_waiting_list.experiment_id = pool_experiments.id
      AND
      pool_waiting_list.contact_id = (SELECT id FROM pool_contacts WHERE user_uuid = UNHEX(REPLACE(?, '-', '')))
  |sql}
  in
  join
  |> select_from_experiments_sql ~distinct:true
  |> Pool_common.Repo.Id.t ->* RepoEntity.Public.t
;;

let find_where_contact_is_on_waitinglist pool contact =
  Utils.Database.collect
    (Pool_database.Label.value pool)
    find_where_contact_is_on_waitinglist_request
    (Contact.id contact)
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
