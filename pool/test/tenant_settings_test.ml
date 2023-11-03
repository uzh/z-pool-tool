module Command = Cqrs_command.Settings_command
module Field = Pool_common.Message.Field
open Settings

let get_or_failwith = Test_utils.get_or_failwith
let database_label = Test_utils.Data.database_label

module Testable = struct
  let contact_email = Settings.ContactEmail.(Alcotest.testable pp equal)
  let email_suffix = Settings.EmailSuffix.(Alcotest.testable pp equal)

  let inactive_user_disable_after =
    Settings.InactiveUser.DisableAfter.(Alcotest.testable pp equal)
  ;;

  let inactive_user_warning =
    Settings.InactiveUser.Warning.(Alcotest.testable pp equal)
  ;;

  let language = Pool_common.Language.(Alcotest.testable pp equal)

  let terms_and_conditions =
    Settings.TermsAndConditions.(Alcotest.testable pp equal)
  ;;
end

let check_events expected generated =
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      generated)
;;

let handle_result result =
  result |> get_or_failwith |> Pool_event.handle_events database_label
;;

let check_contact_email _ () =
  let open CCResult.Infix in
  let open Command.UpdateContactEmail in
  let field = Field.ContactEmail in
  let handle email = [ Field.show field, [ email ] ] |> decode >>= handle in
  let invalid_email = "email.example.com" in
  let result = handle invalid_email in
  let expected =
    Error Pool_common.Message.(Conformist [ field, Invalid field ])
  in
  let () = check_events expected result in
  let valid_email = "pool@econ.uzh.ch" in
  let result = handle valid_email in
  let expected_email = ContactEmail.of_string valid_email in
  let expected =
    Ok [ ContactEmailUpdated expected_email |> Pool_event.settings ]
  in
  let () = check_events expected result in
  let%lwt () = handle_result result in
  let%lwt contact_email = Settings.find_contact_email database_label in
  let () =
    Alcotest.(
      check
        Testable.contact_email
        "contact email address"
        contact_email
        expected_email)
  in
  Lwt.return ()
;;

let check_email_suffix _ () =
  let open CCResult.Infix in
  let open Command in
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
  let%lwt () = handle_result result in
  let%lwt suffixes = find_email_suffixes database_label in
  let expected = EmailSuffix.of_string suffix |> CCList.return in
  let () =
    Alcotest.(
      check
        (list Testable.email_suffix)
        "created email suffix"
        suffixes
        expected)
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
  let%lwt () = handle_result result in
  let%lwt suffixes = find_email_suffixes database_label in
  let expected = EmailSuffix.of_string updated |> CCList.return in
  let () =
    Alcotest.(
      check
        (list Testable.email_suffix)
        "updated email suffix"
        suffixes
        expected)
  in
  Lwt.return ()
;;

let check_inactive_user_disable_after _ () =
  let open CCResult.Infix in
  let open Command.InactiveUser in
  let field = Field.InactiveUserDisableAfter in
  let handle nr =
    DisableAfter.(
      [ Field.(show field), [ CCInt.to_string nr ] ] |> decode >>= handle)
  in
  let result = handle (-1) in
  let expected =
    Error Pool_common.Message.(Conformist [ field, TimeSpanPositive ])
  in
  let () = check_events expected result in
  let valid = 365 in
  let result = handle valid in
  let expected =
    Ok
      [ Settings.InactiveUserDisableAfterUpdated
          (InactiveUser.DisableAfter.create (CCInt.to_string valid)
           |> get_or_failwith)
        |> Pool_event.settings
      ]
  in
  let () = check_events expected result in
  let%lwt () = handle_result result in
  let%lwt disable_after =
    Settings.find_inactive_user_disable_after database_label
  in
  let expected =
    valid
    |> CCInt.to_string
    |> InactiveUser.DisableAfter.create
    |> get_or_failwith
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
  let open CCResult.Infix in
  let open Command.InactiveUser in
  let field = Field.InactiveUserWarning in
  let handle nr =
    Warning.(
      [ Field.(show field), [ CCInt.to_string nr ] ] |> decode >>= handle)
  in
  let result = handle (-1) in
  let expected =
    Error Pool_common.Message.(Conformist [ field, TimeSpanPositive ])
  in
  let () = check_events expected result in
  let valid = 365 in
  let result = handle valid in
  let expected =
    Ok
      [ Settings.InactiveUserWarningUpdated
          (InactiveUser.Warning.create (CCInt.to_string valid)
           |> get_or_failwith)
        |> Pool_event.settings
      ]
  in
  let () = check_events expected result in
  let%lwt () = handle_result result in
  let%lwt warning_after = Settings.find_inactive_user_warning database_label in
  let expected =
    valid |> CCInt.to_string |> InactiveUser.Warning.create |> get_or_failwith
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

let check_languages _ () =
  let%lwt language = Settings.find_languages database_label in
  Alcotest.(
    check
      (list Testable.language)
      "languages"
      language
      Pool_common.Language.[ En; De ])
  |> Lwt.return
;;

let check_terms_and_conditions _ () =
  let open Settings in
  let%lwt terms = find_terms_and_conditions database_label in
  let has_terms = terms |> CCList.is_empty |> not in
  Alcotest.(check bool "has terms and conditions" has_terms true) |> Lwt.return
;;

let update_terms_and_conditions _ () =
  let%lwt languages = Settings.find_languages database_label in
  let terms_and_conditions_text = "Terms and conditions" in
  let%lwt events =
    let open Utils.Lwt_result.Infix in
    let data =
      CCList.map
        (fun lang ->
          Pool_common.Language.show lang, [ terms_and_conditions_text ])
        languages
    in
    let result =
      Cqrs_command.Settings_command.UpdateTermsAndConditions.handle
        languages
        data
      |> Lwt_result.lift
    in
    let%lwt (_ : (unit Lwt.t, Pool_common.Message.error) result) =
      result >|+ Lwt_list.iter_s (Pool_event.handle_event database_label)
    in
    result
  in
  let expected =
    let open CCResult.Infix in
    let* terms_and_conditions =
      CCResult.flatten_l
        (CCList.map
           (fun l ->
             Settings.TermsAndConditions.create
               (l |> Pool_common.Language.show)
               terms_and_conditions_text)
           languages)
    in
    Ok
      [ Settings.TermsAndConditionsUpdated terms_and_conditions
        |> Pool_event.settings
      ]
  in
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      events)
  |> Lwt.return
;;

let login_after_terms_update _ () =
  let open Utils.Lwt_result.Infix in
  let open Pool_common.Message in
  let%lwt user = Integration_utils.ContactRepo.create () in
  let accepted =
    let contact =
      Contact.find_by_email database_label (Contact.email_address user)
    in
    let terms_agreed contact =
      let%lwt accepted = Contact.has_terms_accepted database_label contact in
      match accepted with
      | true -> Lwt.return_ok contact
      | false -> Lwt.return_error TermsAndConditionsNotAccepted
    in
    contact >|- CCFun.const (NotFound Field.Contact) >>= terms_agreed
  in
  let expected = Error TermsAndConditionsNotAccepted in
  accepted
  ||> fun accepted ->
  Alcotest.(
    check
      (result Test_utils.contact Test_utils.error)
      "succeeds"
      expected
      accepted)
;;
