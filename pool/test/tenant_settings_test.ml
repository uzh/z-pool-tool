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

let database_label = Test_utils.Data.database_label

let check_contact_email _ () =
  let%lwt contact = Settings.find_contact_email database_label in
  let expected =
    Settings.ContactEmail.create "pool@econ.uzh.ch"
    |> Test_utils.get_or_failwith_pool_error
  in
  Alcotest.(
    check Testable.contact_email "contact email address" contact expected)
  |> Lwt.return
;;

let check_email_suffix _ () =
  let open Settings in
  let%lwt suffix = find_email_suffixes database_label in
  Alcotest.(
    check bool "has minimum one email suffix" (suffix |> CCList.is_empty) false)
  |> Lwt.return
;;

let check_inactive_user_disable_after _ () =
  let%lwt disable = Settings.find_inactive_user_disable_after database_label in
  let expected =
    Settings.InactiveUser.DisableAfter.create "5"
    |> Test_utils.get_or_failwith_pool_error
  in
  Alcotest.(
    check
      Testable.inactive_user_disable_after
      "disable inactive user after weeks"
      disable
      expected)
  |> Lwt.return
;;

let check_inactive_user_warning _ () =
  let%lwt warning = Settings.find_inactive_user_warning database_label in
  let expected =
    Settings.InactiveUser.Warning.create "7"
    |> Test_utils.get_or_failwith_pool_error
  in
  Alcotest.(
    check
      Testable.inactive_user_warning
      "inactive user warning after days"
      warning
      expected)
  |> Lwt.return
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

let login_after_terms_update _ () =
  let email = "one@test.com" in
  let accepted =
    let open Utils.Lwt_result.Infix in
    let participant =
      Participant.find_by_email
        database_label
        (email |> Pool_user.EmailAddress.of_string)
    in
    let terms_agreed participant =
      let%lwt accepted =
        Participant.has_terms_accepted database_label participant
      in
      match accepted with
      | true -> Lwt.return_ok participant
      | false ->
        Lwt.return_error Pool_common.Message.(TermsAndConditionsNotAccepted)
    in
    participant
    |> Lwt_result.map_err
         (CCFun.const Pool_common.Message.(NotFound Participant))
    >>= terms_agreed
  in
  let expected = Error Pool_common.Message.(TermsAndConditionsNotAccepted) in
  accepted
  |> Lwt.map (fun accepted ->
         Alcotest.(
           check
             (result Test_utils.participant Test_utils.error)
             "succeeds"
             expected
             accepted))
;;
