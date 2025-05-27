open Tyxml.Html

let account_suspension_notification ?unblock_url { Pool_context.csrf; language; _ } =
  let open Pool_user.FailedLoginAttempt in
  function
  | None -> txt ""
  | Some { blocked_until; _ } ->
    let unblock =
      match unblock_url with
      | None -> txt ""
      | Some unblock_url ->
        let open Component_input in
        form
          ~a:[ a_method `Post; a_action (unblock_url |> Sihl.Web.externalize_path) ]
          [ csrf_element csrf ()
          ; submit_element
              ~classnames:[ "small" ]
              ~is_text:true
              ~has_icon:Component_icon.Close
              ~submit_type:`Error
              language
              Pool_message.Control.Unblock
              ()
          ]
    in
    blocked_until
    |> CCOption.map_or ~default:(txt "") (fun blocked_until ->
      [ div
          ~a:
            [ a_class
                [ "flexrow"; "justify-between"; "align-center"; "flexcolumn-mobile" ]
            ]
          [ span
              [ txt
                  Pool_common.(
                    Utils.text_to_string
                      language
                      I18n.(UserLoginBlockedUntil (BlockedUntil.value blocked_until)))
              ]
          ; unblock
          ]
      ]
      |> Component_notification.create language `Error)
;;
