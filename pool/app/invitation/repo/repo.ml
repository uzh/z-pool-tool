open CCFun.Infix
module RepoEntity = Repo_entity
module Dynparam = Database.Dynparam

let sql_select_columns =
  (Pool_common.Id.sql_select_fragment ~field:"pool_invitations.uuid"
   :: Contact.Repo.sql_select_columns)
  @ [ "pool_invitations.resent_at"
    ; "pool_invitations.send_count"
    ; "pool_invitations.created_at"
    ; "pool_invitations.updated_at"
    ]
;;

let joins =
  Format.asprintf
    {sql|
      INNER JOIN pool_contacts
        ON pool_invitations.contact_uuid = pool_contacts.user_uuid
      %s
    |sql}
    Contact.Repo.joins
;;

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
             |> Dynparam.add Contact.Repo.Id.t (invitation.Entity.contact |> Contact.id)
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
    Database.exec pool prepare_request pv
  ;;
end

let find_request_sql ?(count = false) where_fragment =
  let columns = if count then "COUNT(*)" else CCString.concat ", " sql_select_columns in
  Format.asprintf
    {sql|SELECT %s FROM pool_invitations %s %s|sql}
    columns
    joins
    where_fragment
;;

let find_request =
  let open Caqti_request.Infix in
  {sql|
    WHERE
      pool_invitations.uuid = UNHEX(REPLACE(?, '-', ''))
  |sql}
  |> find_request_sql
  |> Caqti_type.string ->! RepoEntity.t
;;

let find pool id =
  let open Utils.Lwt_result.Infix in
  Database.find_opt pool find_request (Pool_common.Id.value id)
  ||> CCOption.to_result Pool_message.(Error.NotFound Field.Tenant)
;;

let find_by_experiment ?query pool id =
  let where = {sql| pool_invitations.experiment_uuid = UNHEX(REPLACE(?, '-', '')) |sql} in
  let dyn = Dynparam.(empty |> add Pool_common.Repo.Id.t (Experiment.Id.to_common id)) in
  Query.collect_and_count pool query ~select:find_request_sql ~where ~dyn Repo_entity.t
;;

let find_by_contact_request =
  let open Caqti_request.Infix in
  {sql| WHERE contact_uuid = UNHEX(REPLACE(?, '-', '')), |sql}
  |> find_request_sql
  |> Contact.Repo.Id.t ->* RepoEntity.t
;;

let find_by_contact pool = Contact.id %> Database.collect pool find_by_contact_request

let find_by_contact_to_merge_request =
  let open Caqti_request.Infix in
  {sql|
    WHERE contact_uuid = UNHEX(REPLACE($1, '-', ''))
    AND NOT EXISTS (
      SELECT 1
      FROM pool_invitations AS merge
      WHERE pool_invitations.experiment_uuid = merge.experiment_uuid
        AND merge.contact_uuid = UNHEX(REPLACE($2, '-', '')))
  |sql}
  |> find_request_sql
  |> Caqti_type.(t2 Contact.Repo.Id.t Contact.Repo.Id.t) ->* RepoEntity.t
;;

let find_by_contact_to_merge pool ~contact ~merged_contact =
  let open Contact in
  Database.collect pool find_by_contact_to_merge_request (id merged_contact, id contact)
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
  Database.find_opt
    pool
    find_experiment_id_of_invitation_request
    (invitation.Entity.id |> Pool_common.Id.value)
  ||> CCOption.map Experiment.Id.of_string
  ||> CCOption.to_result Pool_message.(Error.NotFound Field.Tenant)
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
  let%lwt () = Database.exec pool resend_request invitation in
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
    (CCList.mapi (fun i _ -> Format.asprintf "UNHEX(REPLACE($%n, '-', ''))" (i + 2)) ids
     |> CCString.concat ",")
;;

let find_multiple_by_experiment_and_contacts pool ids experiment =
  let open Caqti_request.Infix in
  let dyn =
    CCList.fold_left
      (fun dyn id -> dyn |> Dynparam.add Contact.Repo.Id.t id)
      (Dynparam.empty |> Dynparam.add Experiment.Repo.Entity.Id.t experiment.Experiment.id)
      ids
  in
  let (Dynparam.Pack (pt, pv)) = dyn in
  let request =
    ids |> find_multiple_by_experiment_and_contacts_request |> pt ->* Contact.Repo.Id.t
  in
  Database.collect pool request pv
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
  let invitations = CCList.map CCFun.(uncurry (fun id -> Entity.create ~id)) contacts in
  let Dynparam.Pack (pt, pv), value_insert =
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
            ?)
          |sql}
         in
         ( dyn
           |> Dynparam.add
                RepoEntity.Write.t
                (entity |> RepoEntity.Write.of_entity experiment_id)
         , sql @ [ sql_line ] ))
      (Dynparam.empty, [])
      invitations
  in
  let prepare_request =
    let open Caqti_request.Infix in
    let values = CCString.concat ",\n" value_insert in
    [%string "%{insert_sql} \n %{values} \n ON DUPLICATE KEY UPDATE updated_at = now()"]
    |> (pt ->. Caqti_type.unit) ~oneshot:true
  in
  let%lwt () = Database.exec pool prepare_request pv in
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
  |> find_request_sql
  |> Caqti_type.t2 Experiment.Repo.Entity.Id.t Contact.Repo.Id.t ->? RepoEntity.t
;;

let find_by_contact_and_experiment_opt pool experiment_id contact_id =
  Database.find_opt
    pool
    find_by_contact_and_experiment_opt_request
    (experiment_id, contact_id)
;;
