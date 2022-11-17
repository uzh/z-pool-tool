open Tyxml.Html
open Component.Input
module Table = Component.Table
module File = Pool_common.File
module Id = Pool_common.Id
module Message = Pool_common.Message

let list tenant_list root_list Pool_context.{ language; csrf; _ } =
  let build_tenant_rows tenant_list =
    let thead = Pool_common.Message.Field.[ Some Tenant; None ] in
    let open Pool_tenant in
    let body =
      CCList.map
        (fun (tenant : Pool_tenant.t) ->
          [ txt (tenant.title |> Pool_tenant.Title.value)
          ; a
              ~a:
                [ a_href
                    (Sihl.Web.externalize_path
                       (Format.asprintf "/root/tenants/%s" (Id.value tenant.id)))
                ]
              [ txt Pool_common.(Utils.control_to_string language Message.More)
              ]
          ])
        tenant_list
    in
    Table.horizontal_table `Striped ~thead language body
  in
  let build_root_rows root_list =
    let open Sihl.Contract.User in
    let status_toggle (status : Sihl.Contract.User.status) id =
      let text, style =
        match status with
        | Active -> Message.Disable, "error"
        | Inactive -> Message.Enable, "primary"
      in
      form
        ~a:
          [ a_action
              (Sihl.Web.externalize_path
                 (Format.asprintf "/root/root/%s/toggle-status" id))
          ; a_method `Post
          ; a_class [ "stack" ]
          ]
        [ submit_element language text ~classnames:[ style ] () ]
    in
    let thead = Pool_common.Message.Field.[ Some Email; None ] in
    let rows =
      CCList.map
        (fun root ->
          let user = root |> Root.user in
          let status = status_toggle user.status user.id in
          [ txt user.email; status ])
        root_list
    in
    Component.Table.horizontal_table `Striped language rows ~thead
  in
  let tenant_list = build_tenant_rows tenant_list in
  let root_list = build_root_rows root_list in
  let text_fields =
    Message.Field.
      [ Title
      ; Description
      ; Url
      ; DatabaseUrl
      ; DatabaseLabel
      ; SmtpAuthServer
      ; SmtpPort
      ; SmtpUsername
      ; SmtpPassword
      ; SmtpAuthMethod
      ; SmtpProtocol
      ]
  in
  let input_fields =
    let language_select =
      let open Pool_common.Language in
      selector language Message.Field.Language show all None ()
    in
    let open Message in
    CCList.map (input_element language `Text) text_fields
    @ [ language_select ]
    @ CCList.map (input_element_file language) [ Field.Styles; Field.Icon ]
    @ CCList.map
        (input_element_file language ~allow_multiple:true)
        [ Field.TenantLogos; Field.PartnerLogos ]
  in
  div
    ~a:[ a_class [ "trim"; "narrow" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt Pool_common.(Utils.nav_link_to_string language I18n.Tenants) ]
    ; div
        ~a:[ a_class [ "stack-lg" ] ]
        [ tenant_list
        ; div
            [ h2
                ~a:[ a_class [ "heading-2" ] ]
                [ Pool_common.(
                    Utils.control_to_string
                      language
                      Message.(Create (Some Field.Tenant)))
                  |> txt
                ]
            ; form
                ~a:
                  [ a_action (Sihl.Web.externalize_path "/root/tenants/create")
                  ; a_method `Post
                  ; a_enctype "multipart/form-data"
                  ; a_class [ "stack" ]
                  ]
                ((csrf_element csrf () :: input_fields)
                @ [ submit_element language Message.(Create None) () ])
            ]
        ; h2 ~a:[ a_class [ "heading-2" ] ] [ txt "Root users" ]
        ; root_list
        ; form
            ~a:
              [ a_action (Sihl.Web.externalize_path "/root/root/create")
              ; a_method `Post
              ; a_class [ "stack" ]
              ]
            (CCList.map
               (input_element language `Text)
               Message.Field.[ Email; Password; Firstname; Lastname ]
            @ [ submit_element language Message.(Create (Some Field.root)) () ]
            )
        ]
    ]
;;

let manage_operators { Pool_tenant.id; _ } Pool_context.{ language; csrf; _ } =
  div
    ~a:[ a_class [ "trim"; "narrow" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt
            Pool_common.(
              Utils.field_to_string language Message.Field.Operators
              |> CCString.capitalize_ascii)
        ]
    ; div
        [ form
            ~a:
              [ a_action
                  (Sihl.Web.externalize_path
                     (Format.asprintf
                        "/root/tenants/%s/create-operator"
                        (Id.value id)))
              ; a_method `Post
              ; a_class [ "stack" ]
              ]
            ((csrf_element csrf ()
             :: CCList.map
                  (fun (field, input) ->
                    input_element ~required:true language input field)
                  Message.Field.
                    [ Email, `Email
                    ; Password, `Password
                    ; Firstname, `Text
                    ; Lastname, `Text
                    ])
            @ [ submit_element
                  language
                  Message.(Create (Some Field.operator))
                  ()
              ])
        ; p
            [ a
                ~a:
                  [ a_href
                      (Sihl.Web.externalize_path
                         (Format.asprintf "/root/tenants/%s" (Id.value id)))
                  ]
                [ txt
                    Pool_common.(Utils.control_to_string language Message.back)
                ]
            ]
        ]
    ]
;;

let detail (tenant : Pool_tenant.t) Pool_context.{ language; csrf; _ } =
  let open Pool_tenant in
  let open Pool_tenant.SmtpAuth in
  let control_to_string = Pool_common.Utils.control_to_string language in
  let detail_fields =
    Message.
      [ Field.Title, Title.value tenant.title
      ; Field.Description, Description.value tenant.description
      ; Field.Url, Pool_tenant.Url.value tenant.url
      ; Field.SmtpAuthServer, Server.value tenant.smtp_auth.server
      ; Field.SmtpPort, Port.value tenant.smtp_auth.port
      ; Field.SmtpUsername, Username.value tenant.smtp_auth.username
      ; ( Field.SmtpAuthMethod
        , AuthenticationMethod.value tenant.smtp_auth.authentication_method )
      ; Field.SmtpProtocol, Protocol.value tenant.smtp_auth.protocol
      ]
  in
  let database_fields =
    Message.
      [ Field.DatabaseUrl, ""
      ; Field.DatabaseLabel, Pool_database.Label.value tenant.database_label
      ]
  in
  let to_input_element (field, value) =
    input_element language `Text field ~value
  in
  let language_select =
    let open Pool_common.Language in
    selector
      language
      Message.Field.Language
      show
      all
      (Some tenant.default_language)
      ()
  in
  let detail_input_fields =
    (CCList.map to_input_element detail_fields @ [ language_select ])
    @ [ div
          [ input_element_file language Message.Field.Styles
          ; div
              ~a:[ a_class [ "gap-xs" ] ]
              [ a
                  ~a:
                    [ a_href
                        (File.path (tenant.styles |> Pool_tenant.Styles.value))
                    ]
                  [ txt "Download" ]
              ]
          ]
      ; div
          [ input_element_file language Message.Field.Icon
          ; div
              ~a:[ a_class [ "gap-xs" ] ]
              [ a
                  ~a:
                    [ a_href (File.path (tenant.icon |> Pool_tenant.Icon.value))
                    ]
                  [ txt "Download" ]
              ]
          ]
      ; input_element_file
          ~allow_multiple:true
          language
          Message.Field.TenantLogos
      ; input_element_file
          ~allow_multiple:true
          language
          Message.Field.PartnerLogos
      ]
  in
  let database_input_fields = CCList.map to_input_element database_fields in
  let disabled =
    checkbox_element
      language
      Message.Field.TenantDisabledFlag
      ~value:(tenant.disabled |> Pool_tenant.Disabled.value)
  in
  let delete_img_form files =
    div
      ~a:[ a_class [ "flexrow" ] ]
      (CCList.map
         (fun (file : File.t) ->
           div
             [ img
                 ~src:(File.path file)
                 ~alt:""
                 ~a:[ a_style "width: 200px" ]
                 ()
             ; form
                 ~a:
                   [ a_action
                       (Sihl.Web.externalize_path
                          (Format.asprintf
                             "/root/tenants/%s/assets/%s/delete"
                             (tenant.id |> Id.value)
                             (File.id file |> Id.value)))
                   ; a_method `Post
                   ; a_class [ "stack" ]
                   ]
                 [ csrf_element csrf ()
                 ; submit_element
                     language
                     Message.(Delete (Some Field.file))
                     ~submit_type:`Error
                     ()
                 ]
             ])
         files)
  in
  let delete_file_forms =
    let label_text m =
      m
      |> Pool_common.Utils.field_to_string language
      |> CCString.capitalize_ascii
    in
    div
      [ h3
          ~a:[ a_class [ "heading-3" ] ]
          [ Message.Field.TenantLogos |> label_text |> txt ]
      ; delete_img_form (tenant.logos |> Pool_tenant.Logos.value)
      ; h3
          ~a:[ a_class [ "heading-3" ] ]
          [ Message.Field.PartnerLogos |> label_text |> txt ]
      ; delete_img_form (tenant.partner_logo |> Pool_tenant.PartnerLogos.value)
      ]
  in
  div
    ~a:[ a_class [ "trim"; "narrow" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt (tenant.Pool_tenant.title |> Pool_tenant.Title.value) ]
    ; p
        [ a
            ~a:
              [ a_href
                  (Sihl.Web.externalize_path
                     (Format.asprintf
                        "/root/tenants/%s/operator"
                        (tenant.id |> Pool_common.Id.value)))
              ]
            [ txt
                (control_to_string Pool_common.Message.(Manage Field.Operators))
            ]
        ]
    ; div
        ~a:[ a_class [ "stack-lg" ] ]
        [ form
            ~a:
              [ a_action
                  (Sihl.Web.externalize_path
                     (Format.asprintf
                        "/root/tenants/%s/update-detail"
                        (Id.value tenant.id)))
              ; a_method `Post
              ; a_enctype "multipart/form-data"
              ; a_class [ "stack" ]
              ]
            ((csrf_element csrf () :: detail_input_fields)
            @ [ disabled; submit_element language Message.(Update None) () ])
        ; delete_file_forms
        ; form
            ~a:
              [ a_action
                  (Sihl.Web.externalize_path
                     (Format.asprintf
                        "/root/tenants/%s/update-database"
                        (Id.value tenant.id)))
              ; a_method `Post
              ; a_enctype "multipart/form-data"
              ; a_class [ "stack" ]
              ]
            ((csrf_element csrf () :: database_input_fields)
            @ [ submit_element language Message.(Update None) () ])
        ; p
            [ a
                ~a:[ a_href (Sihl.Web.externalize_path "/root/tenants") ]
                [ txt (control_to_string Pool_common.Message.back) ]
            ]
        ]
    ]
;;
