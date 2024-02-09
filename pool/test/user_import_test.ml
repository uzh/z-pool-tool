open Pool_common.Message

module Data = struct
  let token = "123123"
  let password = "Password1!"
end

let get_exn = Test_utils.get_or_failwith

let create_user_import ?(token = Data.token) user =
  let open User_import in
  let user_uuid =
    let open Pool_context in
    match user with
    | Contact contact -> Contact.(id contact)
    | Admin admin -> Admin.(id admin |> Id.value |> Pool_common.Id.of_string)
    | Guest -> failwith "Invalid user"
  in
  { user_uuid
  ; token = Token.create token |> get_exn
  ; confirmed_at = None
  ; notified_at = None
  ; reminder_count = ReminderCount.init
  ; last_reminded_at = None
  ; created_at = Pool_common.CreatedAt.create ()
  ; updated_at = Pool_common.UpdatedAt.create ()
  }
;;

let confirm_without_matching_password () =
  let contact =
    Test_utils.Model.create_contact ~with_terms_accepted:false ()
    |> Pool_context.contact
  in
  let user_import = create_user_import contact in
  let urlencoded =
    let open Field in
    [ Token |> show, [ Data.token ]
    ; Password |> show, [ Data.password ]
    ; PasswordConfirmation |> show, [ "Password2!" ]
    ]
  in
  let result =
    let open CCResult in
    let open Cqrs_command.User_import_command.ConfirmImport in
    urlencoded |> decode >>= handle (user_import, contact)
  in
  let expected = Error PasswordConfirmationDoesNotMatch in
  Test_utils.check_result expected result
;;

let confirm_as_contact () =
  let contact = Test_utils.Model.create_contact ~with_terms_accepted:false () in
  let user = contact |> Pool_context.contact in
  let user_import = create_user_import user in
  let urlencoded =
    let open Field in
    [ Token |> show, [ Data.token ]
    ; Password |> show, [ Data.password ]
    ; PasswordConfirmation |> show, [ Data.password ]
    ]
  in
  let result =
    let open CCResult in
    let open Cqrs_command.User_import_command.ConfirmImport in
    urlencoded |> decode >>= handle (user_import, user)
  in
  let expected =
    Ok
      [ Contact.ImportConfirmed
          (contact, Pool_user.Password.create Data.password |> get_exn)
        |> Pool_event.contact
      ; User_import.Confirmed user_import |> Pool_event.user_import
      ]
  in
  Test_utils.check_result expected result
;;

let confirm_as_admin () =
  let user = Test_utils.Model.create_admin () in
  let admin =
    let open Pool_context in
    match user with
    | Admin admin -> admin
    | Guest | Contact _ -> failwith "Invalid user"
  in
  let user_import = create_user_import user in
  let urlencoded =
    let open Field in
    [ Token |> show, [ Data.token ]
    ; Password |> show, [ Data.password ]
    ; PasswordConfirmation |> show, [ Data.password ]
    ]
  in
  let result =
    let open CCResult in
    let open Cqrs_command.User_import_command.ConfirmImport in
    urlencoded |> decode >>= handle (user_import, user)
  in
  let expected =
    Ok
      [ Admin.ImportConfirmed
          (admin, Pool_user.Password.create Data.password |> get_exn)
        |> Pool_event.admin
      ; User_import.Confirmed user_import |> Pool_event.user_import
      ]
  in
  Test_utils.check_result expected result
;;

let confirm_as_contact_integration _ () =
  let%lwt contact =
    Integration_utils.ContactRepo.create ~with_terms_accepted:false ()
  in
  let user = contact |> Pool_context.contact in
  let user_import = create_user_import user in
  let urlencoded =
    let open Field in
    [ Token |> show, [ Data.token ]
    ; Password |> show, [ Data.password ]
    ; PasswordConfirmation |> show, [ Data.password ]
    ]
  in
  let%lwt () =
    let open CCResult in
    let open Cqrs_command.User_import_command.ConfirmImport in
    urlencoded
    |> decode
    >>= handle (user_import, user)
    |> get_exn
    |> Pool_event.handle_events Test_utils.Data.database_label
  in
  let%lwt contact =
    Contact.find Test_utils.Data.database_label (Contact.id contact)
    |> Lwt.map get_exn
  in
  let () =
    Alcotest.(
      check
        bool
        "succeeds"
        false
        (Pool_user.ImportPending.value contact.Contact.import_pending))
  in
  let () =
    Alcotest.(
      check
        bool
        "succeeds"
        true
        (contact.Contact.terms_accepted_at |> CCOption.is_some))
  in
  Lwt.return_unit
;;

module Repo = struct
  open Utils.Lwt_result.Infix

  let set_contact_import_pending pool id =
    let request =
      let open Caqti_request.Infix in
      {sql|
          UPDATE
            pool_contacts
          SET
            import_pending = 1
          WHERE
            user_uuid = UNHEX(REPLACE($1, '-', ''))
        |sql}
      |> Caqti_type.(string ->. unit)
    in
    Utils.Database.exec
      (Pool_database.Label.value pool)
      request
      (Contact.Id.value id)
  ;;

  let set_import_timestamp_to_past pool days id =
    let request =
      let open Caqti_request.Infix in
      {sql|
          UPDATE
            pool_user_imports
          SET
            notification_sent_at = notification_sent_at - INTERVAL $2 DAY
          WHERE
            user_uuid = UNHEX(REPLACE($1, '-', ''))
        |sql}
      |> Caqti_type.(t2 string int ->. unit)
    in
    Utils.Database.exec
      (Pool_database.Label.value pool)
      request
      (Contact.Id.value id, days)
  ;;

  type testable_import = Contact.t * User_import.t [@@deriving eq, show]

  let user_import = Alcotest.testable pp_testable_import equal_testable_import

  let create_contact =
    Integration_utils.ContactRepo.create ~with_terms_accepted:true
  ;;

  let limit = 5
  let database_label = Test_utils.Data.database_label
  let contact_id_1 = Contact.Id.create ()
  let contact_id_2 = Contact.Id.create ()
  let sort_testable = CCList.sort (fun (c1, _) (c2, _) -> Contact.compare c1 c2)

  let reminder_settings database_label =
    let open Settings in
    Lwt.both
      (find_user_import_first_reminder_after database_label)
      (find_user_import_second_reminder_after database_label)
  ;;

  let init () =
    let token () = Pool_common.Id.(() |> create |> value) in
    let%lwt contact1 = create_contact ~id:contact_id_1 () in
    let%lwt contact2 = create_contact ~id:contact_id_2 () in
    let import1 =
      create_user_import ~token:(token ()) (Pool_context.Contact contact1)
    in
    let import2 =
      create_user_import ~token:(token ()) (Pool_context.Contact contact2)
    in
    let%lwt () =
      [ contact_id_1; contact_id_2 ]
      |> Lwt_list.iter_s (set_contact_import_pending database_label)
    in
    [ import2; import1 ]
    |> Lwt_list.iter_s (fun import -> User_import.insert database_label import)
  ;;

  let import_of_contact contact_id =
    Lwt.both
      (Contact.find database_label contact_id ||> get_exn)
      (User_import.find_pending_by_user_id_opt database_label contact_id
       ||> CCOption.get_exn_or "Import not found")
  ;;

  let find_contacts_to_notify _ () =
    let%lwt () = init () in
    let%lwt contacts_to_notify =
      User_import.find_contacts_to_notify database_label limit ()
      ||> sort_testable
    in
    let%lwt expected =
      [ contact_id_1; contact_id_2 ]
      |> Lwt_list.map_s import_of_contact
      ||> sort_testable
    in
    let () =
      Alcotest.(check (list user_import) "succeeds" expected contacts_to_notify)
    in
    let%lwt () =
      contacts_to_notify
      |> CCList.map (fun (_, import) -> User_import.Notified import)
      |> Lwt_list.iter_s (User_import.handle_event database_label)
    in
    (* Expect list to be empty *)
    let%lwt contacts_to_notify =
      User_import.find_contacts_to_notify database_label limit ()
    in
    let () =
      Alcotest.(check (list user_import) "succeeds" [] contacts_to_notify)
    in
    Lwt.return_unit
  ;;

  let find_contacts_to_remind _ () =
    let%lwt reminder_settings = reminder_settings database_label in
    let%lwt contacts_to_remind =
      User_import.find_contacts_to_remind
        reminder_settings
        database_label
        limit
        ()
    in
    (* Expect list to be empty *)
    let () =
      Alcotest.(check (list user_import) "succeeds" [] contacts_to_remind)
    in
    let%lwt () =
      [ contact_id_1; contact_id_2 ]
      |> Lwt_list.iter_s (set_import_timestamp_to_past database_label 8)
    in
    (* Expect both imports to be returned *)
    let%lwt contacts_to_remind =
      User_import.find_contacts_to_remind
        reminder_settings
        database_label
        limit
        ()
      ||> sort_testable
    in
    let%lwt expected =
      [ contact_id_1; contact_id_2 ]
      |> Lwt_list.map_s import_of_contact
      ||> sort_testable
    in
    let () =
      Alcotest.(check (list user_import) "succeeds" expected contacts_to_remind)
    in
    (* Expect list to be empty with increased duration *)
    let first_reminder_after =
      Settings.UserImportReminder.FirstReminderAfter.of_int_s (60 * 60 * 24 * 10)
      |> get_exn
    in
    let%lwt contacts_to_remind =
      User_import.find_contacts_to_remind
        (first_reminder_after, snd reminder_settings)
        database_label
        limit
        ()
    in
    let () =
      Alcotest.(check (list user_import) "succeeds" [] contacts_to_remind)
    in
    Lwt.return_unit
  ;;
end
