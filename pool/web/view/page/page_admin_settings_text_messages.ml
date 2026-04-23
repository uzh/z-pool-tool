open Tyxml.Html
open Component.Input

let index
      { Pool_context.csrf; language; flash_fetcher; _ }
      (gtx_config : Gtx_config.t option)
      phone_verification_enabled
  =
  let open Pool_common in
  let open Gtx_config in
  let action = "/admin/settings/text-messages" |> Sihl.Web.externalize_path in
  let delete_form, hint =
    let notification hint style =
      Utils.hint_to_string language hint
      |> txt
      |> CCList.return
      |> Component.Notification.create language style
    in
    match gtx_config with
    | None -> txt "", notification I18n.GtxKeyMissing `Warning
    | Some _ ->
      ( form
          ~a:
            [ a_action (Sihl.Web.externalize_path (Format.asprintf "%s/delete" action))
            ; a_method `Post
            ; a_class [ "stack" ]
            ; a_user_data
                "confirmable"
                (Utils.confirmable_to_string language I18n.DeleteGtxApiKey)
            ]
          [ csrf_element csrf ()
          ; submit_element
              ~submit_type:`Error
              ~has_icon:Icon.TrashOutline
              language
              Pool_message.(Control.Delete (Some Field.GtxApiKey))
              ()
          ]
      , notification I18n.GtxKeyStored `Success )
  in
  let phone_verification_form =
    let phone_verification_action = Format.asprintf "%s/phone-verification" action in
    let disabled = CCOption.is_none gtx_config in
    div
      ~a:[ a_class [ "stack" ] ]
      [ h2
          ~a:[ a_class [ "heading-2" ] ]
          [ txt
              (Utils.field_to_string language Pool_message.Field.PhoneVerificationEnabled
               |> CCString.capitalize_ascii)
          ]
      ; p [ txt (Utils.hint_to_string language I18n.PhoneVerificationHint) ]
      ; form
          ~a:[ a_action phone_verification_action; a_method `Post; a_class [ "stack" ] ]
          [ csrf_element csrf ()
          ; checkbox_element
              ~as_switch:true
              ~disabled
              ~value:phone_verification_enabled
              ?flash_fetcher
              language
              Pool_message.Field.PhoneVerificationEnabled
          ; div
              ~a:[ a_class [ "flexrow" ] ]
              [ submit_element
                  ~classnames:[ "push" ]
                  ~attributes:(if disabled then [ a_disabled () ] else [])
                  language
                  Pool_message.(Control.Update (Some Field.PhoneVerificationEnabled))
                  ()
              ]
          ]
      ]
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1"; "has-gap" ] ]
        [ txt (Utils.nav_link_to_string language I18n.TextMessages) ]
    ; div
        ~a:[ a_class [ "stack-lg" ] ]
        [ div
            ~a:[ a_class [ "stack" ] ]
            [ h2
                ~a:[ a_class [ "heading-2" ] ]
                [ txt (Utils.field_to_string language Field.GtxApiKey) ]
            ; hint
            ; form
                ~a:[ a_action action; a_method `Post; a_class [ "stack" ] ]
                [ csrf_element csrf ()
                ; input_element
                    language
                    `Text
                    Field.GtxApiKey
                    ?flash_fetcher
                    ~required:true
                ; input_element
                    language
                    `Text
                    Field.TestPhoneNumber
                    ?flash_fetcher
                    ~required:true
                    ~hints:[ I18n.TestPhoneNumber ]
                ; input_element
                    language
                    `Text
                    Field.GtxSender
                    ?value:
                      (CCOption.map (fun { sender; _ } -> Sender.value sender) gtx_config)
                    ?flash_fetcher
                    ~required:true
                    ~hints:[ I18n.GtxSender ]
                ; submit_element
                    language
                    Pool_message.(Control.Update (Some Field.GtxApiKey))
                    ()
                ]
            ; delete_form
            ]
        ; phone_verification_form
        ]
    ]
;;
