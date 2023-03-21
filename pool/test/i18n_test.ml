module I18nCommand = Cqrs_command.I18n_command
module SettingsCommand = Cqrs_command.Settings_command
module Common = Pool_common

let database_label = Test_utils.Data.database_label

let update_terms_and_conditions () =
  let languages = Pool_common.Language.[ En; De ] in
  let events =
    [ "EN", [ "Terms and Conditions" ]; "DE", [ "Nutzungsbedingungen" ] ]
    |> Cqrs_command.Settings_command.UpdateTermsAndConditions.handle languages
  in
  let expected =
    let open CCResult in
    let* terms =
      [ "EN", "Terms and Conditions"; "DE", "Nutzungsbedingungen" ]
      |> CCList.map (fun (language, content) ->
           Settings.TermsAndConditions.create language content)
      |> CCResult.flatten_l
    in
    Ok [ Settings.TermsAndConditionsUpdated terms |> Pool_event.settings ]
  in
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      events)
;;
