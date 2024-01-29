open Tyxml.Html
open Component.Input

let[@warning "-27"] index
  { Pool_context.csrf; language; _ }
  ~flash_fetcher
  tenant
  =
  let open Pool_common in
  let action = "/admin/settings/text-messages" |> Sihl.Web.externalize_path in
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "measure" ] ]
    [ h1 [ txt (Utils.field_to_string language Field.GtxApiKey) ]
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
            ~hints:[ Pool_common.I18n.TestPhoneNumber ]
        ; submit_element language Message.(Update (Some Field.GtxApiKey)) ()
        ]
    ]
;;
