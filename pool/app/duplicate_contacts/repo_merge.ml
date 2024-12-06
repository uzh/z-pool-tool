open Entity
open Utils.Lwt_result.Infix
open Caqti_request.Infix
open Caqti_type
module Dynparam = Database.Dynparam

let id_select_fragment = Pool_common.Id.sql_select_fragment
let id_value_fragment = Pool_common.Id.sql_value_fragment

let update_queue =
  {sql|
    UPDATE pool_queue_jobs_mapping
    SET entity_uuid = UNHEX(REPLACE($1, '-', ''))
    WHERE entity_uuid = UNHEX(REPLACE($2, '-', ''))
    |sql}
  |> Contact.Repo.Id.(t2 t t ->. unit)
;;

let update_changelog =
  {sql|
    UPDATE pool_change_log
    SET entity_uuid = UNHEX(REPLACE($1, '-', ''))
    WHERE entity_uuid = UNHEX(REPLACE($2, '-', ''))
    |sql}
  |> Contact.Repo.Id.(t2 t t ->. unit)
;;

let update_tags =
  {sql|
    INSERT IGNORE INTO pool_tagging (model_uuid, tag_uuid)
    SELECT UNHEX(REPLACE($1, '-', '')), tag_uuid
    FROM pool_tagging
    WHERE model_uuid = UNHEX(REPLACE($2, '-', ''));
  |sql}
  |> Contact.Repo.Id.(t2 t t ->. unit)
;;

let uuid_sql dyn items to_id =
  let dyn, sql =
    CCList.foldi
      (fun (dyn, sql) i item ->
         ( dyn |> Dynparam.add Caqti_type.string (to_id item)
         , sql @ [ Format.asprintf "$%d" (i + 2) ] ))
      (dyn, [])
      items
  in
  dyn, CCString.concat "," sql
;;

let update_invitations contact_id invitations =
  let dyn = Dynparam.(empty |> add Contact.Repo.Id.t contact_id) in
  let dyn, sql =
    uuid_sql dyn invitations (fun { Invitation.id; _ } ->
      Pool_common.Id.value id)
  in
  let sql =
    Format.asprintf
      {sql|
        UPDATE pool_invitations
        SET contact_uuid = UNHEX(REPLACE($1, '-', ''))
        WHERE UUID IN (%s);
      |sql}
      sql
  in
  sql, dyn
;;

let update_waiting_list contact_id waiting_list =
  let dyn = Dynparam.(empty |> add Contact.Repo.Id.t contact_id) in
  let dyn, sql =
    uuid_sql dyn waiting_list (fun { Waiting_list.id; _ } ->
      Waiting_list.Id.value id)
  in
  let sql =
    Format.asprintf
      {sql|
        UPDATE pool_waiting_list
        SET contact_uuid = UNHEX(REPLACE($1, '-', ''))
        WHERE UUID IN (%s);
      |sql}
      sql
  in
  sql, dyn
;;

let update_assignments contact_id assignments =
  let dyn = Dynparam.(empty |> add Contact.Repo.Id.t contact_id) in
  let dyn, sql =
    uuid_sql dyn assignments (fun { Assignment.id; _ } ->
      Assignment.Id.value id)
  in
  let sql =
    Format.asprintf
      {sql|
        UPDATE pool_assignments
        SET contact_uuid = UNHEX(REPLACE($1, '-', ''))
        WHERE UUID IN (%s);
      |sql}
      sql
  in
  sql, dyn
;;

let destroy_tags =
  {sql|
    DELETE FROM pool_tagging
    WHERE model_uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
  |> Contact.Repo.Id.t ->. unit
;;

let destroy_mailing_invitations =
  {sql|
    DELETE FROM pool_mailing_invitations
    WHERE invitation_uuid IN (
      SELECT uuid FROM pool_invitations WHERE contact_uuid = UNHEX(REPLACE($1, '-', ''))
    )
  |sql}
  |> Contact.Repo.Id.t ->. unit
;;

let destroy_invitations =
  {sql|
    DELETE FROM pool_invitations
    WHERE contact_uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
  |> Contact.Repo.Id.t ->. unit
;;

let destroy_waiting_lists =
  {sql|
    DELETE FROM pool_waiting_list
    WHERE contact_uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
  |> Contact.Repo.Id.t ->. unit
;;

let destroy_assignments =
  {sql|
    DELETE FROM pool_assignments
    WHERE contact_uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
  |> Contact.Repo.Id.t ->. unit
;;

(** CHANGELOGS

    - Contact
    - waiting_list
    - invitations
    - assignments
    - Taggins ?? *)

module Changelog = struct
  let contact_changelog pool current_contact_state contact =
    let open Contact in
    let open VersionHistory in
    insert
      pool
      ~entity_uuid:(id contact |> Id.to_common)
      ~before:current_contact_state
      ~after:contact
      ()
  ;;
end

let merge
      pool
      { contact; merged_contact; kept_fields }
      invitations
      waiting_list
      assignments
  =
  let open Contact in
  let* current_contact_state = find pool (id contact) in
  let update_contact connection =
    let (module Connection : Caqti_lwt.CONNECTION) = connection in
    Connection.exec Repo.update_request (to_write contact)
  in
  let update_user connection =
    let (module Connection : Caqti_lwt.CONNECTION) = connection in
    Connection.exec Pool_user.Repo.update_request contact.user
  in
  let store_custom_answers =
    let entity_uuid = id contact |> Id.to_common in
    kept_fields
    |> CCList.map (fun field connection ->
      let (module Connection : Caqti_lwt.CONNECTION) = connection in
      Custom_field.Repo.override_answer ~entity_uuid field
      |> function
      | `Clear (req, arg) -> Connection.exec req arg
      | `Override (req, arg) -> Connection.exec req arg)
  in
  let override_queue connection =
    let (module Connection : Caqti_lwt.CONNECTION) = connection in
    Connection.exec update_queue (id contact, id merged_contact)
  in
  let override_changelog connection =
    let (module Connection : Caqti_lwt.CONNECTION) = connection in
    Connection.exec update_changelog (id contact, id merged_contact)
  in
  let override_tags connection =
    let (module Connection : Caqti_lwt.CONNECTION) = connection in
    Connection.exec update_tags (id contact, id merged_contact)
  in
  let exec_dyn_request connection (sql, dyn) =
    let (module Connection : Caqti_lwt.CONNECTION) = connection in
    let (Dynparam.Pack (pt, pv)) = dyn in
    let request = sql |> pt ->. unit in
    Connection.exec request pv
  in
  let override_invitations =
    match invitations with
    | [] -> None
    | invitations ->
      Some
        (fun connection ->
          update_invitations (id contact) invitations
          |> exec_dyn_request connection)
  in
  let override_waiting_list =
    match waiting_list with
    | [] -> None
    | waiting_list ->
      Some
        (fun connection ->
          update_waiting_list (id contact) waiting_list
          |> exec_dyn_request connection)
  in
  let override_assignments =
    match assignments with
    | [] -> None
    | assignments ->
      Some
        (fun connection ->
          update_assignments (id contact) assignments
          |> exec_dyn_request connection)
  in
  let destroy_tags connection =
    let (module Connection : Caqti_lwt.CONNECTION) = connection in
    Connection.exec destroy_tags (id merged_contact)
  in
  let destroy_mailing_invitations connection =
    let (module Connection : Caqti_lwt.CONNECTION) = connection in
    Connection.exec destroy_mailing_invitations (id merged_contact)
  in
  let destroy_invitations connection =
    let (module Connection : Caqti_lwt.CONNECTION) = connection in
    Connection.exec destroy_invitations (id merged_contact)
  in
  let destroy_waiting_lists connection =
    let (module Connection : Caqti_lwt.CONNECTION) = connection in
    Connection.exec destroy_waiting_lists (id merged_contact)
  in
  let destroy_assignments connection =
    let (module Connection : Caqti_lwt.CONNECTION) = connection in
    Connection.exec destroy_assignments (id merged_contact)
  in
  let actions =
    [ override_invitations; override_waiting_list; override_assignments ]
    |> CCList.filter_map CCFun.id
  in
  let destroy_requests =
    [ destroy_tags
    ; destroy_mailing_invitations
    ; destroy_invitations
    ; destroy_waiting_lists
    ; destroy_assignments
    ]
  in
  let%lwt () =
    Database.transaction_iter
      pool
      ([ update_contact
       ; update_user
       ; override_queue
       ; override_changelog
       ; override_tags
       ]
       @ store_custom_answers
       @ actions
       @ destroy_requests)
  in
  let create_changelog () =
    let%lwt () =
      Changelog.contact_changelog pool current_contact_state contact
    in
    let%lwt () =
      kept_fields
      |> Lwt_list.iter_s
           (Custom_field.create_custom_field_answer_changelog pool contact)
    in
    Lwt.return ()
  in
  () |> create_changelog |> Lwt_result.ok
;;
