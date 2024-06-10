open Tyxml.Html
open Component.Input

let index { Pool_context.csrf; language; _ } ~flash_fetcher tenant =
  let open Pool_common in
  let action = "/admin/settings/text-messages" |> Sihl.Web.externalize_path in
  let delete_form, hint =
    let notification hint style =
      Utils.hint_to_string language hint
      |> txt
      |> CCList.return
      |> Component.Notification.notification language style
    in
    match tenant.Pool_tenant.text_messages_enabled with
    | false -> txt "", notification I18n.GtxKeyMissing `Warning
    | true ->
      ( form
          ~a:
            [ a_action
                (Sihl.Web.externalize_path (Format.asprintf "%s/delete" action))
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
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "measure" ] ]
    [ h1 [ txt (Utils.field_to_string language Field.GtxApiKey) ]
    ; div
        ~a:[ a_class [ "stack-lg" ] ]
        [ hint
        ; form
            ~a:
              [ a_action (Sihl.Web.externalize_path action)
              ; a_method `Post
              ; a_class [ "stack" ]
              ]
            [ csrf_element csrf ()
            ; input_element
                language
                `Text
                Field.GtxApiKey
                ~flash_fetcher
                ~required:true
            ; input_element
                language
                `Text
                Field.TestPhoneNumber
                ~flash_fetcher
                ~required:true
                ~hints:[ I18n.TestPhoneNumber ]
            ; input_element
                language
                `Text
                Field.GtxSender
                ~value:Pool_tenant.(tenant.gtx_sender |> GtxSender.value)
                ~flash_fetcher
                ~required:true
                ~hints:[ I18n.GtxSender ]
            ; submit_element
                language
                Pool_message.(Control.Update (Some Field.GtxApiKey))
                ()
            ]
        ; delete_form
        ]
    ]
;;
