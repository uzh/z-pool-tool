open Pool_message
open Settings
module Command = Cqrs_command.Settings_command

let get_or_failwith = Test_utils.get_or_failwith
let database_label = Test_utils.Data.database_label
let current_user = Test_utils.Model.create_admin ()
let days_to_timespan days = days * 60 * 60 * 24 |> Ptime.Span.of_int_s

module Testable = struct
  let contact_email = Settings.ContactEmail.(Alcotest.testable pp equal)
  let email_suffix = Settings.EmailSuffix.(Alcotest.testable pp equal)

  let inactive_user_disable_after =
    Settings.InactiveUser.DisableAfter.(Alcotest.testable pp equal)
  ;;

  let inactive_user_warning = Settings.InactiveUser.Warning.(Alcotest.testable pp equal)
  let language = Pool_common.Language.(Alcotest.testable pp equal)
  let terms_and_conditions = Settings.TermsAndConditions.(Alcotest.testable pp equal)
end

let check_events ?(message = "succeeds") expected generated =
  Alcotest.(
    check (result (list Test_utils.event) Test_utils.error) message expected generated)
;;

let handle_result ?(current_user = current_user) result =
  result |> get_or_failwith |> Pool_event.handle_events database_label current_user
;;

let check_contact_email _ () =
  let open CCResult.Infix in
  let open Command.UpdateContactEmail in
  let%lwt current_user = Integration_utils.create_admin_user () in
  let field = Field.ContactEmail in
  let handle email = [ Field.show field, [ email ] ] |> decode >>= handle in
  let invalid_email = "email.example.com" in
  let result = handle invalid_email in
  let expected = Error Error.(Conformist [ field, Invalid field ]) in
  let () = check_events ~message:"invalid contact email" expected result in
  let valid_email = "pool@econ.uzh.ch" in
  let result = handle valid_email in
  let expected_email = ContactEmail.of_string valid_email in
  let expected = Ok [ ContactEmailUpdated expected_email |> Pool_event.settings ] in
  let () = check_events ~message:"valid contact email" expected result in
  let%lwt () = handle_result ~current_user result in
  let%lwt contact_email = Settings.find_contact_email database_label in
  let () =
    Alcotest.(
      check Testable.contact_email "contact email address" contact_email expected_email)
  in
  Lwt.return ()
;;

let check_email_suffix _ () =
  let open CCResult.Infix in
  let open Command in
  let%lwt current_user =
    Integration_utils.AdminRepo.create () |> Lwt.map Pool_context.admin
  in
  let field = Field.EmailSuffix in
  let suffix = "econ.uzh.ch" in
  let handle_create suffix =
    CreateEmailSuffix.([ Field.show field, [ suffix ] ] |> decode >>= handle [])
  in
  let result = handle_create suffix in
  let expected =
    Ok
      [ Settings.EmailSuffixesUpdated [ EmailSuffix.of_string suffix ]
        |> Pool_event.settings
      ]
  in
  let () = check_events expected result in
  let%lwt () = handle_result ~current_user result in
  let%lwt suffixes = find_email_suffixes database_label in
  let expected = EmailSuffix.of_string suffix |> CCList.return in
  let () =
    Alcotest.(check (list Testable.email_suffix) "created email suffix" suffixes expected)
  in
  let handle_updated suffix =
    UpdateEmailSuffixes.([ Field.show field, [ suffix ] ] |> handle)
  in
  let updated = "uzh.ch" in
  let result = handle_updated updated in
  let expected =
    Ok
      [ Settings.EmailSuffixesUpdated [ EmailSuffix.of_string updated ]
        |> Pool_event.settings
      ]
  in
  let () = check_events expected result in
  let%lwt () = handle_result ~current_user result in
  let%lwt suffixes = find_email_suffixes database_label in
  let expected = EmailSuffix.of_string updated |> CCList.return in
  let () =
    Alcotest.(check (list Testable.email_suffix) "updated email suffix" suffixes expected)
  in
  Lwt.return ()
;;

let check_inactive_user_disable_after _ () =
  let open CCResult.Infix in
  let open Command.InactiveUser in
  let%lwt current_user =
    Integration_utils.AdminRepo.create () |> Lwt.map Pool_context.admin
  in
  let field = Field.InactiveUserDisableAfter in
  let handle nr =
    DisableAfter.(
      Field.
        [ show field, [ CCInt.to_string nr ]
        ; (show (TimeUnitOf field), Pool_model.Base.TimeUnit.[ show Days ])
        ]
      |> decode
      >>= handle)
  in
  let result = handle (-1) in
  let expected = Error Error.NegativeAmount in
  let () = check_events expected result in
  let valid = 365 in
  let result = handle valid in
  let expected =
    Ok
      [ Settings.InactiveUserDisableAfterUpdated
          (days_to_timespan valid |> InactiveUser.DisableAfter.create |> get_or_failwith)
        |> Pool_event.settings
      ]
  in
  let () = check_events expected result in
  let%lwt () = handle_result ~current_user result in
  let%lwt disable_after = Settings.find_inactive_user_disable_after database_label in
  let expected =
    days_to_timespan valid |> InactiveUser.DisableAfter.create |> get_or_failwith
  in
  let () =
    Alcotest.(
      check
        Testable.inactive_user_disable_after
        "inavtive user disable after"
        disable_after
        expected)
  in
  Lwt.return ()
;;

let check_inactive_user_warning _ () =
  let open Command.InactiveUser in
  let%lwt current_user =
    Integration_utils.AdminRepo.create () |> Lwt.map Pool_context.admin
  in
  let handle nr =
    let values = [ CCInt.to_string nr ] in
    let units = Pool_model.Base.TimeUnit.[ show Days ] in
    Warning.handle ~values ~units ()
  in
  let result = handle (-1) in
  let expected = Error Error.NegativeAmount in
  let () = check_events expected result in
  let valid = 365 in
  let result = handle valid in
  let expected =
    Ok
      [ Settings.InactiveUserWarningUpdated
          (valid
           |> days_to_timespan
           |> InactiveUser.Warning.TimeSpan.create
           |> get_or_failwith
           |> CCList.return)
        |> Pool_event.settings
      ]
  in
  let () = check_events expected result in
  let%lwt () = handle_result ~current_user result in
  let%lwt warning_after = Settings.find_inactive_user_warning database_label in
  let expected =
    valid
    |> days_to_timespan
    |> InactiveUser.Warning.TimeSpan.create
    |> get_or_failwith
    |> CCList.return
  in
  let () =
    Alcotest.(
      check
        Testable.inactive_user_warning
        "inavtive user warning after"
        warning_after
        expected)
  in
  Lwt.return ()
;;

let disable_inactive_user_service _ () =
  let open Command.InactiveUser in
  let%lwt current_user =
    Integration_utils.AdminRepo.create () |> Lwt.map Pool_context.admin
  in
  let command disabled =
    let open CCResult.Infix in
    let values =
      [ ( Pool_message.Field.(show InactiveUserDisableService)
        , [ Pool_model.Base.Boolean.stringify disabled ] )
      ]
    in
    DisableService.decode values >>= DisableService.handle
  in
  let event disabled =
    Settings.InactiveUserServiceDisabled
      (Settings.InactiveUser.ServiceDisabled.create disabled)
    |> Pool_event.settings
  in
  let handle_events events =
    events |> get_or_failwith |> Pool_event.handle_events database_label current_user
  in
  let open Utils.Lwt_result.Infix in
  let run_test disable =
    let msg = if disable then "disabled" else "enabled" in
    let events = command disable in
    let () =
      Test_utils.check_result
        ~msg:(Format.asprintf "%s service events" msg)
        events
        (Ok [ event disable ])
    in
    let%lwt () = handle_events events in
    let%lwt disabled =
      Settings.find_inactive_user_service_disabled database_label
      ||> InactiveUser.ServiceDisabled.value
    in
    Alcotest.(check bool) (Format.asprintf "Service is %s" msg) disable disabled
    |> Lwt.return
  in
  let%lwt () = run_test true in
  let%lwt () = run_test false in
  Lwt.return_unit
;;

let check_languages _ () =
  let%lwt language = Settings.find_languages database_label in
  Alcotest.(
    check (list Testable.language) "languages" language Pool_common.Language.[ En; De ])
  |> Lwt.return
;;

let check_terms_and_conditions _ () =
  let%lwt terms =
    Lwt_list.map_s
      (I18n.find_by_key database_label I18n.Key.TermsAndConditions)
      Pool_common.Language.all
  in
  let has_terms = terms |> CCList.is_empty |> not in
  Alcotest.(check bool "has terms and conditions" has_terms true) |> Lwt.return
;;

let login_after_terms_update _ () =
  let open Utils.Lwt_result.Infix in
  let%lwt user = Integration_utils.ContactRepo.create () in
  let accepted =
    let contact = Contact.find_by_email database_label (Contact.email_address user) in
    let terms_agreed contact =
      let%lwt accepted = Contact.has_terms_accepted database_label contact in
      match accepted with
      | true -> Lwt.return_ok contact
      | false -> Lwt.return_error Error.TermsAndConditionsNotAccepted
    in
    contact >|- CCFun.const (Error.NotFound Field.Contact) >>= terms_agreed
  in
  let expected = Error Error.TermsAndConditionsNotAccepted in
  accepted
  ||> fun accepted ->
  Alcotest.(
    check (result Test_utils.contact Test_utils.error) "succeeds" expected accepted)
;;

(* Test the creation of an SMTP Authentication record. *)
let create_smtp_auth =
  Test_utils.case
  @@ fun () ->
  let ( let* ) x f = Lwt_result.bind (Lwt_result.lift x) f in
  let ( let& ) = Lwt_result.bind in
  let test_db = Test_utils.Data.database_label in
  let open Email.SmtpAuth in
  (* create an smtp auth instance *)
  let id = Id.create () in
  let* label = Label.create ("a-label-" ^ Id.show id) in
  let* server = Server.create "a-server" in
  let* port = Port.create 2112 in
  let* username = Username.create "a-username" in
  let* password = Password.create "Password1!" in
  let mechanism = Mechanism.PLAIN in
  let protocol = Protocol.SSL_TLS in
  let default = Default.create false in
  let* write_event =
    Email.SmtpAuth.Write.create
      ~id
      label
      server
      port
      (Some username)
      (Some password)
      mechanism
      protocol
      default
  in
  (* feed the event into the event handler to affect the database *)
  let& () =
    [ Email.SmtpCreated write_event |> Pool_event.email ]
    |> Pool_event.handle_events test_db current_user
    |> Lwt_result.ok
  in
  (* get the smtp auth that was actually saved and compare it *)
  let expected = Email.SmtpAuth.Write.to_entity write_event in
  let& actual = Email.SmtpAuth.find test_db id in
  Alcotest.(check Test_utils.smtp_auth "smtp saved correctly" expected actual);
  Lwt.return_ok ()
;;

(* Test the deletion of an SMTP Authentication record. *)
let delete_smtp_auth =
  Test_utils.case
  @@ fun () ->
  let ( let* ) x f = Lwt_result.bind (Lwt_result.lift x) f in
  let ( let& ) = Lwt_result.bind in
  let ( let^ ) = Lwt.bind in
  let test_db = Test_utils.Data.database_label in
  let open Email.SmtpAuth in
  (* create an smtp auth instance *)
  let id = Id.create () in
  let* label = Label.create ("a-label-" ^ Id.show id) in
  let* server = Server.create "a-server" in
  let* port = Port.create 2112 in
  let* username = Username.create "a-username" in
  let* password = Password.create "Password1!" in
  let mechanism = Mechanism.PLAIN in
  let protocol = Protocol.SSL_TLS in
  let default = Default.create false in
  let* write_event =
    Email.SmtpAuth.Write.create
      ~id
      label
      server
      port
      (Some username)
      (Some password)
      mechanism
      protocol
      default
  in
  (* feed the event into the event handler to affect the database *)
  let& () =
    [ Email.SmtpCreated write_event |> Pool_event.email
    ; Email.SmtpDeleted id |> Pool_event.email
    ]
    |> Pool_event.handle_events test_db current_user
    |> Lwt_result.ok
  in
  (* get the smtp auth that was actually saved and compare it *)
  let^ res = Email.SmtpAuth.find test_db id in
  Alcotest.(
    check
      (result Test_utils.smtp_auth Test_utils.error)
      "the auth record was not deleted"
      res
      (Error (Error.NotFound Field.Smtp)));
  Lwt.return_ok ()
;;

let update_gtx_settings _ () =
  let testable_gtx =
    let open Gtx_config in
    let equal (key1, sender1) (key2, sender2) =
      ApiKey.equal key1 key2 && Sender.equal sender1 sender2
    in
    let pp fmt (key, sender) =
      Format.fprintf fmt "(%s, %s)" (ApiKey.show key) (Sender.show sender)
    in
    Alcotest.testable pp equal
  in
  let tags = Database.Logger.Tags.create Test_utils.Data.database_label in
  let validate = Cqrs_command.Settings_command.validated_gtx_api_key ~tags in
  let phone = "+41791234567" in
  let stringify = CCList.map (fun (field, value) -> Field.show field, value) in
  let urlencoded = [ Field.GtxApiKey, [ "api-key" ]; Field.TestPhoneNumber, [ phone ] ] in
  let%lwt res = urlencoded |> stringify |> validate in
  let expected = Error Error.(Conformist [ Field.GtxSender, NoValue ]) in
  let () =
    Alcotest.(check (result testable_gtx Test_utils.error) "succeeds" expected res)
  in
  let urlencoded =
    [ Field.GtxApiKey, [ "api-key" ]
    ; Field.TestPhoneNumber, [ phone ]
    ; Field.GtxSender, [ "longerthanelevenchars" ]
    ]
  in
  let%lwt res = urlencoded |> stringify |> validate in
  let expected = Error Error.(Conformist [ Field.GtxSender, MaxLength 11 ]) in
  let () =
    Alcotest.(check (result testable_gtx Test_utils.error) "succeeds" expected res)
  in
  Lwt.return_unit
;;

let update_page_scripts () =
  let open Settings in
  let open CCResult.Infix in
  let script = "console.log('hello world')" in
  let to_urlencoded ?(script = script) location =
    let field =
      let open PageScript in
      match location with
      | Head -> Field.PageScriptsHead
      | Body -> Field.PageScriptsBody
    in
    [ Field.show field, [ script ] ]
  in
  let open Command.UpdatePageScript in
  let system_event_id = System_event.Id.create () in
  let cache_event =
    System_event.(Job.I18nPageUpdated |> create ~id:system_event_id |> created)
    |> Pool_event.system_event
  in
  let handle location urlencoded =
    urlencoded
    |> Http_utils.remove_empty_values
    |> decode location
    >>= handle ~system_event_id location
  in
  let script = PageScript.of_string script in
  (* Head Script *)
  let location = PageScript.Head in
  let result = location |> to_urlencoded |> handle location in
  let expected =
    [ Settings.PageScriptUpdated (Some script, location) |> Pool_event.settings
    ; cache_event
    ]
  in
  let () = check_events (Ok expected) result in
  let result = location |> to_urlencoded ~script:"" |> handle location in
  let expected =
    [ Settings.PageScriptUpdated (None, location) |> Pool_event.settings; cache_event ]
  in
  let () = check_events (Ok expected) result in
  (* Body Script *)
  let location = PageScript.Body in
  let result = location |> to_urlencoded |> handle location in
  let expected =
    [ Settings.PageScriptUpdated (Some script, location) |> Pool_event.settings
    ; cache_event
    ]
  in
  let () = check_events (Ok expected) result in
  (* Body Script *)
  let result = location |> to_urlencoded ~script:"" |> handle location in
  let expected =
    [ Settings.PageScriptUpdated (None, location) |> Pool_event.settings; cache_event ]
  in
  let () = check_events (Ok expected) result in
  ()
;;
