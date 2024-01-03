module I18nCommand = Cqrs_command.I18n_command
module SettingsCommand = Cqrs_command.Settings_command
module Common = Pool_common

let database_label = Test_utils.Data.database_label

let update_terms_and_conditions () =
  let languages = Pool_common.Language.[ En; De ] in
  let content = "Terms and Conditions" in
  let terms =
    CCList.map
      (fun lang ->
        I18n.create
          I18n.Key.TermsAndConditions
          lang
          (I18n.Content.of_string content))
      languages
  in
  let events =
    let open CCResult in
    let open I18nCommand.Update in
    terms
    |> CCList.map (fun i18n ->
      [ Common.Message.Field.(show Translation), [ content ] ]
      |> decode
      >>= handle i18n)
    |> CCResult.flatten_l
    |> CCResult.map CCList.flatten
  in
  let expected =
    terms
    |> CCList.map (fun i18n ->
      let content = I18n.Content.of_string content in
      [ I18n.Updated (i18n, content) |> Pool_event.i18n ])
    |> CCList.flatten
    |> CCResult.return
  in
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      events)
;;
