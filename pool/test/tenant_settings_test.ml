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

module Data = struct
  let database_label = "econ-test" |> Pool_database.Label.of_string
end

let check_contact_email _ () =
  let%lwt contact = Settings.find_contact_email Data.database_label in
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
  let%lwt suffix = find_email_suffixes Data.database_label in
  Alcotest.(
    check bool "has minimum one email suffix" (suffix |> CCList.is_empty) false)
  |> Lwt.return
;;

let check_inactive_user_disable_after _ () =
  let%lwt disable =
    Settings.find_inactive_user_disable_after Data.database_label
  in
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
  let%lwt warning = Settings.find_inactive_user_warning Data.database_label in
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
  let%lwt language = Settings.find_languages Data.database_label in
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
  let%lwt terms = find_terms_and_conditions Data.database_label in
  let has_terms =
    terms |> TermsAndConditions.value |> CCString.is_empty |> not
  in
  Alcotest.(check bool "has terms and conditions" has_terms true) |> Lwt.return
;;
