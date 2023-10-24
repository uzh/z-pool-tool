module RepoEntity = Repo_entity
module Dynparam = Utils.Database.Dynparam
module Statistics = Repo_statistics

let to_entity = RepoEntity.to_entity
let of_entity = RepoEntity.of_entity

module Sql = struct
  module MailingInvitationMapping = struct
    let bulk_insert pool invitations mailing_id =
      let insert_sql =
        {sql|
          INSERT INTO pool_mailing_invitations (
            mailing_uuid,
            invitation_uuid
          ) VALUES
        |sql}
      in
      let values, value_insert =
        CCList.fold_left
          (fun (dyn, sql) invitation ->
            let sql_line =
              {sql| ( UNHEX(REPLACE(?, '-', '')),
                (SELECT uuid FROM pool_invitations WHERE experiment_uuid = (SELECT experiment_uuid FROM pool_mailing WHERE uuid = UNHEX(REPLACE(?, '-', ''))) AND contact_uuid = UNHEX(REPLACE(?, '-', '')))
              ) |sql}
            in
            ( dyn
              |> Dynparam.add Mailing.Repo.Id.t mailing_id
              |> Dynparam.add Mailing.Repo.Id.t mailing_id
              |> Dynparam.add
                   Pool_common.Repo.Id.t
                   (invitation.Entity.contact |> Contact.id)
            , sql @ [ sql_line ] ))
          (Dynparam.empty, [])
          invitations
      in
      let (Dynparam.Pack (pt, pv)) = values in
      let prepare_request =
        let open Caqti_request.Infix in
        Format.asprintf
          {sql|
            %s
            %s
            ON DUPLICATE KEY UPDATE updated_at = NOW()
          |sql}
          insert_sql
          (CCString.concat ",\n" value_insert)
        |> (pt ->. Caqti_type.unit) ~oneshot:true
      in
      Utils.Database.exec (pool |> Pool_database.Label.value) prepare_request pv
    ;;
  end

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
          pool_invitations.send_count,
          pool_invitations.created_at,
          pool_invitations.updated_at
        FROM
          pool_invitations
        LEFT JOIN pool_contacts
          ON pool_invitations.contact_uuid = pool_contacts.user_uuid
        LEFT JOIN user_users
          ON pool_contacts.user_uuid = user_users.uuid
        LEFT JOIN pool_experiments
          ON pool_invitations.experiment_uuid = pool_experiments.uuid
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
          ON pool_invitations.contact_uuid = pool_contacts.user_uuid
        LEFT JOIN user_users
          ON pool_contacts.user_uuid = user_users.uuid
        LEFT JOIN pool_experiments
          ON pool_invitations.experiment_uuid = pool_experiments.uuid
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

  let find_by_experiment ?query pool id =
    let where =
      let sql =
        {sql| pool_invitations.experiment_uuid = UNHEX(REPLACE(?, '-', '')) |sql}
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
        contact_uuid = UNHEX(REPLACE(?, '-', '')),
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

  let find_binary_experiment_id_sql =
    {sql|
      SELECT inv.experiment_uuid
      FROM pool_invitations AS inv
      WHERE inv.uuid = ?
    |sql}
  ;;

  let find_experiment_id_of_invitation_request =
    let open Caqti_request.Infix in
    {sql|
      SELECT
        LOWER(CONCAT(
          SUBSTR(HEX(pool_invitations.experiment_uuid), 1, 8), '-',
          SUBSTR(HEX(pool_invitations.experiment_uuid), 9, 4), '-',
          SUBSTR(HEX(pool_invitations.experiment_uuid), 13, 4), '-',
          SUBSTR(HEX(pool_invitations.experiment_uuid), 17, 4), '-',
          SUBSTR(HEX(pool_invitations.experiment_uuid), 21)
        ))
      FROM
        pool_invitations
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

  let resend_request =
    let open Caqti_request.Infix in
    {sql|
      UPDATE pool_invitations
      SET
        resent_at = $2,
        send_count = $3
      WHERE uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> RepoEntity.Update.t ->. Caqti_type.unit
  ;;

  let resend ?mailing_id pool invitation =
    let%lwt () =
      Utils.Database.exec
        (Pool_database.Label.value pool)
        resend_request
        invitation
    in
    CCOption.map_or
      ~default:Lwt.return_unit
      (MailingInvitationMapping.bulk_insert pool [ invitation ])
      mailing_id
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
        ON pool_contacts.user_uuid = pool_invitations.contact_uuid
      WHERE
      pool_invitations.experiment_uuid = UNHEX(REPLACE($1, '-', ''))
      AND
        pool_invitations.contact_uuid IN ( %s )
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

  let bulk_insert ?mailing_id pool contacts experiment_id =
    let insert_sql =
      {sql|
        INSERT INTO pool_invitations (
          uuid,
          experiment_uuid,
          contact_uuid,
          resent_at,
          send_count,
          created_at,
          updated_at
        ) VALUES
      |sql}
    in
    let invitations =
      CCList.map CCFun.(uncurry (fun id -> Entity.create ~id)) contacts
    in
    let values, value_insert =
      CCList.fold_left
        (fun (dyn, sql) entity ->
          let sql_line =
            {sql| (
              UNHEX(REPLACE(?, '-', '')),
              UNHEX(REPLACE(?, '-', '')),
              UNHEX(REPLACE(?, '-', '')),
              UNHEX(REPLACE(?, '-', '')),
              ?,
              ?,
              ?
            ) |sql}
          in
          ( dyn |> Dynparam.add RepoEntity.t (entity |> of_entity experiment_id)
          , sql @ [ sql_line ] ))
        (Dynparam.empty, [])
        invitations
    in
    let (Dynparam.Pack (pt, pv)) = values in
    let prepare_request =
      let open Caqti_request.Infix in
      Format.asprintf
        {sql|
          %s
          %s
        |sql}
        insert_sql
        (CCString.concat ",\n" value_insert)
      |> (pt ->. Caqti_type.unit) ~oneshot:true
    in
    let%lwt () =
      Utils.Database.exec (pool |> Pool_database.Label.value) prepare_request pv
    in
    let%lwt () =
      CCOption.map_or
        ~default:Lwt.return_unit
        (MailingInvitationMapping.bulk_insert pool invitations)
        mailing_id
    in
    Lwt.return_unit
  ;;

  let find_by_contact_and_experiment_opt_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE
        experiment_uuid = UNHEX(REPLACE(?, '-', ''))
      AND
        contact_uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> select_sql
    |> Caqti_type.(tup2 string string) ->? RepoEntity.t
  ;;

  let find_by_contact_and_experiment_opt pool experiment_id contact_id =
    Utils.Database.find_opt
      (Pool_database.Label.value pool)
      find_by_contact_and_experiment_opt_request
      (Experiment.Id.value experiment_id, Contact.Id.value contact_id)
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

let resend = Sql.resend
let bulk_insert = Sql.bulk_insert

let find_by_contact_and_experiment_opt pool experiment_id contact_id =
  let open Utils.Lwt_result.Infix in
  Sql.find_by_contact_and_experiment_opt pool experiment_id contact_id
  >|> function
  | None -> Lwt_result.return None
  | Some invitation -> contact_to_invitation pool invitation >|+ CCOption.return
;;
