module RepoEntity = Repo_entity
module Dynparam = Utils.Database.Dynparam

let to_entity = RepoEntity.to_entity
let of_entity = RepoEntity.of_entity

module Sql = struct
  let select_sql =
    Format.asprintf
      {sql|
        SELECT
          LOWER(CONCAT(
            SUBSTR(HEX(pool_invitations.uuid), 1, 8), '-',
            SUBSTR(HEX(pool_invitations.uuid), 9, 4), '-',
            SUBSTR(HEX(pool_invitations.uuid), 13, 4), '-',
            SUBSTR(HEX(pool_invitations.uuid), 17, 4), '-',
            SUBSTR(HEX(pool_invitations.uuid), 21)
          )),
          LOWER(CONCAT(
            SUBSTR(HEX(pool_experiments.uuid), 1, 8), '-',
            SUBSTR(HEX(pool_experiments.uuid), 9, 4), '-',
            SUBSTR(HEX(pool_experiments.uuid), 13, 4), '-',
            SUBSTR(HEX(pool_experiments.uuid), 17, 4), '-',
            SUBSTR(HEX(pool_experiments.uuid), 21)
          )),
          LOWER(CONCAT(
            SUBSTR(HEX(pool_contacts.user_uuid), 1, 8), '-',
            SUBSTR(HEX(pool_contacts.user_uuid), 9, 4), '-',
            SUBSTR(HEX(pool_contacts.user_uuid), 13, 4), '-',
            SUBSTR(HEX(pool_contacts.user_uuid), 17, 4), '-',
            SUBSTR(HEX(pool_contacts.user_uuid), 21)
          )),
          pool_invitations.resent_at,
          pool_invitations.created_at,
          pool_invitations.updated_at
        FROM
          pool_invitations
        LEFT JOIN pool_contacts
          ON pool_invitations.contact_id = pool_contacts.id
        LEFT JOIN user_users
          ON pool_contacts.user_uuid = user_users.uuid
        LEFT JOIN pool_experiments
          ON pool_invitations.experiment_id = pool_experiments.id
        %s
      |sql}
  ;;

  let select_count =
    Format.asprintf
      {sql|
        SELECT COUNT(*)
        FROM
          pool_invitations
        LEFT JOIN pool_contacts
          ON pool_invitations.contact_id = pool_contacts.id
        LEFT JOIN user_users
          ON pool_contacts.user_uuid = user_users.uuid
        LEFT JOIN pool_experiments
          ON pool_invitations.experiment_id = pool_experiments.id
        %s
      |sql}
  ;;

  let find_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE
        pool_invitations.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> select_sql
    |> Caqti_type.string ->! RepoEntity.t
  ;;

  let find pool id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt
      (Pool_database.Label.value pool)
      find_request
      (Pool_common.Id.value id)
    ||> CCOption.to_result Pool_common.Message.(NotFound Field.Tenant)
  ;;

  let find_by_experiment_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE
        experiment_id = (SELECT id FROM pool_experiments WHERE uuid = UNHEX(REPLACE(?, '-', '')))
    |sql}
    |> select_sql
    |> Caqti_type.string ->* RepoEntity.t
  ;;

  let find_by_experiment ?query pool id =
    let where =
      let sql =
        {sql| experiment_id = (SELECT id FROM pool_experiments WHERE uuid = UNHEX(REPLACE(?, '-', ''))) |sql}
      in
      let dyn =
        Dynparam.(
          empty |> add Pool_common.Repo.Id.t (Experiment.Id.to_common id))
      in
      sql, dyn
    in
    Query.collect_and_count
      pool
      query
      ~select:select_sql
      ~count:select_count
      ~where
      Repo_entity.t
  ;;

  let find_by_contact_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE
        contact_id = (SELECT id FROM pool_contacts WHERE user_uuid = UNHEX(REPLACE(?, '-', ''))),
    |sql}
    |> select_sql
    |> Caqti_type.string ->* RepoEntity.t
  ;;

  let find_by_contact pool id =
    Utils.Database.collect
      (Pool_database.Label.value pool)
      find_by_contact_request
      (Pool_common.Id.value id)
  ;;

  let find_experiment_id_of_invitation_request =
    let open Caqti_request.Infix in
    {sql|
      SELECT
        LOWER(CONCAT(
          SUBSTR(HEX(pool_experiments.uuid), 1, 8), '-',
          SUBSTR(HEX(pool_experiments.uuid), 9, 4), '-',
          SUBSTR(HEX(pool_experiments.uuid), 13, 4), '-',
          SUBSTR(HEX(pool_experiments.uuid), 17, 4), '-',
          SUBSTR(HEX(pool_experiments.uuid), 21)
        ))
      FROM
        pool_invitations
      LEFT JOIN pool_experiments
        ON pool_invitations.session_id = pool_experiments.id
      WHERE
        pool_invitations.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> Caqti_type.(string ->! string)
  ;;

  let find_experiment_id_of_invitation pool invitation =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt
      (Pool_database.Label.value pool)
      find_experiment_id_of_invitation_request
      (invitation.Entity.id |> Pool_common.Id.value)
    ||> CCOption.map Experiment.Id.of_string
    ||> CCOption.to_result Pool_common.Message.(NotFound Field.Tenant)
  ;;

  let update_request =
    let open Caqti_request.Infix in
    {sql|
      UPDATE pool_invitations
      SET
        resent_at = $2
      WHERE uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> RepoEntity.Update.t ->. Caqti_type.unit
  ;;

  let update pool =
    Utils.Database.exec (Pool_database.Label.value pool) update_request
  ;;

  let contact_was_invited_to_experiment_request =
    let open Caqti_request.Infix in
    {sql|
      SELECT pool_invitations.uuid
      FROM
        pool_invitations
      WHERE
        (SELECT id FROM pool_experiments WHERE pool_experiments.uuid = UNHEX(REPLACE($1, '-', ''))),
        (SELECT id FROM pool_contacts WHERE pool_contacts.user_uuid = UNHEX(REPLACE($2, '-', '')))
      LIMIT 1
      |sql}
    |> select_sql
    |> Caqti_type.(tup2 string string) ->! Pool_common.Repo.Id.t
  ;;

  let contact_was_invited_to_experiment pool experiment contact =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt
      (Pool_database.Label.value pool)
      contact_was_invited_to_experiment_request
      ( experiment.Experiment.id |> Experiment.Id.value
      , Contact.id contact |> Pool_common.Id.value )
    ||> CCOption.is_some
  ;;

  let find_multiple_by_experiment_and_contacts_request ids =
    Format.asprintf
      {sql|
      SELECT
        LOWER(CONCAT(
          SUBSTR(HEX(user_users.uuid), 1, 8), '-',
          SUBSTR(HEX(user_users.uuid), 9, 4), '-',
          SUBSTR(HEX(user_users.uuid), 13, 4), '-',
          SUBSTR(HEX(user_users.uuid), 17, 4), '-',
          SUBSTR(HEX(user_users.uuid), 21)
        ))
      FROM user_users
      INNER JOIN pool_contacts
        ON pool_contacts.user_uuid = user_users.uuid
      INNER JOIN pool_invitations
        ON pool_contacts.id = pool_invitations.contact_id
      WHERE
      pool_invitations.experiment_id = (SELECT id FROM pool_experiments WHERE pool_experiments.uuid = UNHEX(REPLACE($1, '-', '')))
      AND
        pool_invitations.contact_id IN ((SELECT id FROM pool_contacts WHERE pool_contacts.user_uuid IN ( %s )))
    |sql}
      (CCList.mapi
         (fun i _ -> Format.asprintf "UNHEX(REPLACE($%n, '-', ''))" (i + 2))
         ids
       |> CCString.concat ",")
  ;;

  let find_multiple_by_experiment_and_contacts pool ids experiment =
    let open Caqti_request.Infix in
    let dyn =
      CCList.fold_left
        (fun dyn id ->
          dyn |> Dynparam.add Caqti_type.string (id |> Pool_common.Id.value))
        (Dynparam.empty
         |> Dynparam.add
              Caqti_type.string
              (experiment.Experiment.id |> Experiment.Id.value))
        ids
    in
    let (Dynparam.Pack (pt, pv)) = dyn in
    let request =
      ids
      |> find_multiple_by_experiment_and_contacts_request
      |> pt ->* Pool_common.Repo.Id.t
    in
    Utils.Database.collect (pool |> Pool_database.Label.value) request pv
  ;;
end

let contact_to_invitation pool invitation =
  let open Utils.Lwt_result.Infix in
  Contact.find pool invitation.RepoEntity.contact_id >|+ to_entity invitation
;;

let find pool id =
  let open Utils.Lwt_result.Infix in
  (* TODO Implement as transaction *)
  Sql.find pool id >>= contact_to_invitation pool
;;

let find_by_experiment ?query pool id =
  let open Utils.Lwt_result.Infix in
  (* TODO Implement as transaction *)
  let%lwt invitations, query = Sql.find_by_experiment ?query pool id in
  invitations
  |> Lwt_list.map_s (contact_to_invitation pool)
  ||> CCList.all_ok
  >|+ fun invitations -> invitations, query
;;

let find_by_contact pool contact =
  let open Utils.Lwt_result.Infix in
  (* TODO Implement as transaction *)
  contact
  |> Contact.id
  |> Sql.find_by_contact pool
  (* Reload contact from DB, does not allow already made updates of the provided
     contact record *)
  >|> Lwt_list.map_s (contact_to_invitation pool)
  ||> CCList.all_ok
;;

let find_experiment_id_of_invitation = Sql.find_experiment_id_of_invitation

let find_multiple_by_experiment_and_contacts =
  Sql.find_multiple_by_experiment_and_contacts
;;

let update = Sql.update
let contact_was_invited_to_experiment = Sql.contact_was_invited_to_experiment

let bulk_insert pool contacts experiment_id =
  let insert_sql =
    {sql|
      INSERT INTO pool_invitations (
        uuid,
        experiment_id,
        contact_id,
        resent_at,
        created_at,
        updated_at
      ) VALUES
    |sql}
  in
  let values, value_insert =
    CCList.fold_left
      (fun (dyn, sql) (id, contact) ->
        let sql_line =
          {sql| (
            UNHEX(REPLACE(?, '-', '')),
            (SELECT id FROM pool_experiments WHERE pool_experiments.uuid = UNHEX(REPLACE(?, '-', ''))),
            (SELECT id FROM pool_contacts WHERE pool_contacts.user_uuid = UNHEX(REPLACE(?, '-', ''))),
            ?,
            ?,
            ?
          ) |sql}
        in
        let entity = contact |> Entity.create ~id |> of_entity experiment_id in
        dyn |> Dynparam.add RepoEntity.t entity, sql @ [ sql_line ])
      (Dynparam.empty, [])
      contacts
  in
  let (Dynparam.Pack (pt, pv)) = values in
  let prepare_request =
    let open Caqti_request.Infix in
    Format.asprintf "%s %s" insert_sql (CCString.concat ",\n" value_insert)
    |> (pt ->. Caqti_type.unit) ~oneshot:true
  in
  Utils.Database.exec (pool |> Pool_database.Label.value) prepare_request pv
;;
