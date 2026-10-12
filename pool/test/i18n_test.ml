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
         let system_event_id = System_event.Id.create () in
         ( I18n.create
             I18n.Key.TermsAndConditions
             lang
             (Some (I18n.Content.of_string content))
         , system_event_id ))
      languages
  in
  let events =
    let open CCResult in
    let open I18nCommand.Update in
    terms
    |> CCList.map (fun (i18n, system_event_id) ->
      [ Pool_message.Field.(show Translation), [ content ] ]
      |> decode
      >>= handle ~system_event_id i18n)
    |> CCResult.flatten_l
    |> CCResult.map CCList.flatten
  in
  let expected =
    terms
    |> CCList.map (fun (i18n, system_event_id) ->
      let content =
        I18n.Content.create_opt content |> Pool_message.Error.get_or_failwith
      in
      [ I18n.Updated (i18n, content) |> Pool_event.i18n
      ; System_event.(
          Job.I18nPageUpdated
          |> create ~id:system_event_id
          |> created
          |> Pool_event.system_event)
      ])
    |> CCList.flatten
    |> CCResult.return
  in
  Alcotest.(
    check (result (list Test_utils.event) Test_utils.error) "succeeds" expected events)
;;

let update_terms_and_conditions_accepts_empty_content () =
  let system_event_id = System_event.Id.create () in
  let i18n =
    I18n.create
      I18n.Key.TermsAndConditions
      Pool_common.Language.En
      (Some (I18n.Content.of_string "Terms and Conditions"))
  in
  let events =
    let open CCResult in
    let open I18nCommand.Update in
    [ Pool_message.Field.(show Translation), [ "" ] ]
    |> decode
    >>= handle ~system_event_id i18n
  in
  let expected =
    Ok
      [ I18n.Updated (i18n, None) |> Pool_event.i18n
      ; System_event.(
          Job.I18nPageUpdated
          |> create ~id:system_event_id
          |> created
          |> Pool_event.system_event)
      ]
  in
  Alcotest.(
    check (result (list Test_utils.event) Test_utils.error) "succeeds" expected events)
;;

let extract_by_key_exn_filters_by_language () =
  let key = I18n.Key.TermsAndConditions in
  let de =
    I18n.create key Pool_common.Language.De (Some (I18n.Content.of_string "Bedingungen"))
  in
  let en =
    I18n.create key Pool_common.Language.En (Some (I18n.Content.of_string "Terms"))
  in
  let extracted = I18n.extract_by_key_exn [ de; en ] key Pool_common.Language.En in
  Alcotest.(
    check bool "extracts entry for requested language" true (I18n.equal extracted en))
;;

let about_page_renders_title_and_text () =
  let i18n key content =
    I18n.create key Pool_common.Language.En (Some (I18n.Content.of_string content))
  in
  let html =
    Page.Public.about_page
      ~title:(i18n I18n.Key.AboutPageTitle "R&D <Lab>")
      ~text:(i18n I18n.Key.AboutPageText "<p>We run <strong>experiments</strong>.</p>")
    |> Format.asprintf "%a" (Tyxml.Html.pp_elt ())
  in
  let contains sub = CCString.find ~sub html >= 0 in
  let open Alcotest in
  check bool "renders escaped title" true (contains "R&amp;D &lt;Lab&gt;</h1>");
  check
    bool
    "renders rich text"
    true
    (contains "<p>We run <strong>experiments</strong>.</p>")
;;

module AboutPage = struct
  let current_user = Test_utils.Model.create_admin ()

  let update ?(language = Pool_common.Language.En) key content =
    let%lwt i18n = I18n.find_by_key database_label key language in
    let%lwt () =
      let open CCResult in
      let open I18nCommand.Update in
      [ Pool_message.Field.(show Translation), [ CCOption.get_or ~default:"" content ] ]
      |> decode
      >>= handle i18n
      |> Test_utils.get_or_failwith
      |> Pool_event.handle_events database_label current_user
    in
    (* the cache is cleared asynchronously by the system event service *)
    I18n.I18nCache.clear () |> Lwt.return
  ;;

  let reset () =
    let open Pool_common.Language in
    Lwt_list.iter_s
      (fun (language, key) -> update ~language key None)
      [ En, I18n.Key.AboutPageTitle
      ; En, I18n.Key.AboutPageText
      ; De, I18n.Key.AboutPageTitle
      ; De, I18n.Key.AboutPageText
      ]
  ;;

  let check_configured ?(language = Pool_common.Language.En) msg expected =
    let%lwt configured = I18n.is_about_page_configured database_label language in
    Alcotest.(check bool msg expected configured) |> Lwt.return
  ;;

  let hidden_by_default _ () =
    let%lwt () = reset () in
    let%lwt () = check_configured ~language:Pool_common.Language.En "en hidden" false in
    check_configured ~language:Pool_common.Language.De "de hidden" false
  ;;

  let requires_title_and_text _ () =
    let open I18n.Key in
    let%lwt () = reset () in
    let%lwt () = update AboutPageTitle (Some "About us") in
    let%lwt () = check_configured "hidden with title only" false in
    let%lwt () = update AboutPageTitle None in
    let%lwt () = update AboutPageText (Some "<p>About us</p>") in
    let%lwt () = check_configured "hidden with text only" false in
    let%lwt () = update AboutPageTitle (Some "About us") in
    let%lwt () = check_configured "shown with title and text" true in
    let%lwt () =
      check_configured ~language:Pool_common.Language.De "other language hidden" false
    in
    let%lwt () = update AboutPageText None in
    let%lwt () = check_configured "hidden again after clearing text" false in
    reset ()
  ;;
end
