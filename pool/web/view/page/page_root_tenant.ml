open Tyxml.Html
module File = Pool_common.File
module Id = Pool_common.Id
module Message = Pool_common.Message

let submit_element = Component.submit_element

let list csrf tenant_list root_list message Pool_context.{ language; _ } =
  let input_element = Component.input_element language in
  let build_tenant_rows tenant_list =
    let open Pool_tenant in
    CCList.map
      (fun (tenant : Pool_tenant.t) ->
        div
          [ h2 [ txt (tenant.title |> Pool_tenant.Title.value) ]
          ; a
              ~a:
                [ a_href
                    (Sihl.Web.externalize_path
                       (Format.asprintf "/root/tenants/%s" (Id.value tenant.id)))
                ]
              [ txt "detail" ]
          ; hr ()
          ])
      tenant_list
  in
  let build_root_rows root_list =
    let open Sihl.Contract.User in
    let status_toggle (status : Sihl.Contract.User.status) id =
      let text, style =
        match status with
        | Active -> Message.Disable, "button--warning"
        | Inactive -> Message.Enable, "button--primary"
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
    CCList.map
      (fun root ->
        let user = root |> Root.user in
        let status = status_toggle user.status user.id in
        div [ h2 [ txt user.email ]; status; hr () ])
      root_list
  in
  let tenant_list = build_tenant_rows tenant_list in
  let root_list = build_root_rows root_list in
  let text_fields =
    let open Message in
    [ "title", Title
    ; "description", Description
    ; "url", Url
    ; "database_url", DatabaseUrl
    ; "database_label", DatabaseLabel
    ; "smtp_auth_server", SmtpAuthServer
    ; "smtp_auth_port", SmtpPort
    ; "smtp_auth_username", SmtpUsername
    ; "smtp_auth_password", SmtpPassword
    ; "smtp_auth_authentication_method", SmtpAuthMethod
    ; "smtp_auth_protocol", SmtpProtocol
    ]
  in
  let input_fields =
    CCList.map
      (fun (name, label) -> input_element `Text (Some name) label "")
      text_fields
    @ Component.language_select
        Pool_common.Language.En
        "language"
        (Pool_common.Language.all ())
        Message.DefaultLanguage
        ~selected:None
        ()
    @ [ div
          [ label [ txt "styles" ]
          ; input ~a:[ a_input_type `File; a_name "styles"; a_value "" ] ()
          ]
      ; div
          [ label [ txt "icon" ]
          ; input ~a:[ a_input_type `File; a_name "icon"; a_value "" ] ()
          ]
      ; div
          [ label [ txt "Add tenant logos" ]
          ; input
              ~a:[ a_input_type `File; a_name "tenant_logo"; a_multiple () ]
              ()
          ]
      ; div
          [ label [ txt "Add partner logos" ]
          ; input
              ~a:[ a_input_type `File; a_name "partner_logo"; a_multiple () ]
              ()
          ]
      ]
  in
  let html =
    div
      [ h1 [ txt "Tenants" ]
      ; div tenant_list
      ; form
          ~a:
            [ a_action (Sihl.Web.externalize_path "/root/tenants/create")
            ; a_method `Post
            ; a_enctype "multipart/form-data"
            ; a_class [ "stack" ]
            ]
          ((Component.csrf_element csrf () :: input_fields)
          @ [ submit_element
                language
                Message.(Create None)
                ~classnames:[ "button--primary" ]
                ()
            ])
      ; hr ()
      ; h1 [ txt "Root users" ]
      ; div root_list
      ; form
          ~a:
            [ a_action (Sihl.Web.externalize_path "/root/root/create")
            ; a_method `Post
            ; a_class [ "stack" ]
            ]
          (CCList.map
             (fun (name, label) -> input_element `Text (Some name) label "")
             [ "email", Message.Email
             ; "password", Message.Password
             ; "firstname", Message.Firstname
             ; "lastname", Message.Lastname
             ]
          @ [ submit_element
                language
                Message.(Create (Some root))
                ~classnames:[ "button--primary" ]
                ()
            ])
      ]
  in
  Page_layout.create_root_layout html message language
;;

let detail csrf (tenant : Pool_tenant.t) message Pool_context.{ language; _ } =
  let open Pool_tenant in
  let open Pool_tenant.SmtpAuth in
  let input_element = Component.input_element language in
  let detail_fields =
    [ "title", Message.Title, Title.value tenant.title
    ; "description", Message.Description, Description.value tenant.description
    ; "url", Message.Url, Pool_tenant.Url.value tenant.url
    ; ( "smtp_auth_server"
      , Message.SmtpAuthServer
      , Server.value tenant.smtp_auth.server )
    ; "smtp_auth_port", Message.SmtpPort, Port.value tenant.smtp_auth.port
    ; ( "smtp_auth_username"
      , Message.SmtpUsername
      , Username.value tenant.smtp_auth.username )
    ; ( "smtp_auth_authentication_method"
      , Message.SmtpAuthMethod
      , AuthenticationMethod.value tenant.smtp_auth.authentication_method )
    ; ( "smtp_auth_protocol"
      , Message.SmtpProtocol
      , Protocol.value tenant.smtp_auth.protocol )
    ]
  in
  let database_fields =
    [ "database_url", Message.DatabaseUrl, ""
    ; ( "database_label"
      , Message.DatabaseLabel
      , Pool_database.Label.value tenant.database_label )
    ]
  in
  let detail_input_fields =
    (CCList.map
       (fun (name, label, value) -> input_element `Text (Some name) label value)
       detail_fields
    @ Component.language_select
        Pool_common.Language.En
        "language"
        (Pool_common.Language.all ())
        ~selected:(Some tenant.default_language)
        Message.DefaultLanguage
        ())
    @ [ div
          [ a
              ~a:
                [ a_href (File.path (tenant.styles |> Pool_tenant.Styles.value))
                ]
              [ txt "styles" ]
          ; input ~a:[ a_input_type `File; a_name "styles" ] ()
          ]
      ; div
          [ a
              ~a:[ a_href (File.path (tenant.icon |> Pool_tenant.Icon.value)) ]
              [ txt "icon" ]
          ; input ~a:[ a_input_type `File; a_name "icon" ] ()
          ]
      ; div
          [ label [ txt "Add tenant logos" ]
          ; input
              ~a:[ a_input_type `File; a_name "tenant_logo"; a_multiple () ]
              ()
          ]
      ; div
          [ label [ txt "Add partner logos" ]
          ; input
              ~a:[ a_input_type `File; a_name "partner_logo"; a_multiple () ]
              ()
          ]
      ]
  in
  let database_input_fields =
    CCList.map
      (fun (name, label, value) -> input_element `Text (Some name) label value)
      database_fields
  in
  let disabled =
    let attributes =
      match tenant.disabled |> Pool_tenant.Disabled.value with
      | true -> [ a_input_type `Checkbox; a_name "disabled"; a_checked () ]
      | false -> [ a_input_type `Checkbox; a_name "disabled" ]
    in
    div [ label [ txt "Disabled" ]; input ~a:attributes () ]
  in
  let delete_img_form files =
    div
      ~a:[ a_style "display: flex;" ]
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
                 [ Component.csrf_element csrf ()
                 ; submit_element
                     language
                     Message.(Delete (Some file))
                     ~classnames:[ "button--failure" ]
                     ()
                 ]
             ])
         files)
  in
  let delete_file_forms =
    div
      [ h3 [ txt "Tenant Logos" ]
      ; delete_img_form (tenant.logos |> Pool_tenant.Logos.value)
      ; h3 [ txt "Partner Logos" ]
      ; delete_img_form (tenant.partner_logo |> Pool_tenant.PartnerLogos.value)
      ]
  in
  let html =
    div
      [ h1 [ txt (tenant.Pool_tenant.title |> Pool_tenant.Title.value) ]
      ; form
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
          ((Component.csrf_element csrf () :: detail_input_fields)
          @ [ disabled
            ; submit_element
                language
                Message.(Update None)
                ~classnames:[ "button--primary" ]
                ()
            ])
      ; hr ()
      ; delete_file_forms
      ; hr ()
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
          ((Component.csrf_element csrf () :: database_input_fields)
          @ [ submit_element
                language
                Message.(Update None)
                ~classnames:[ "button--primary" ]
                ()
            ])
      ; hr ()
      ; form
          ~a:
            [ a_action
                (Sihl.Web.externalize_path
                   (Format.asprintf
                      "/root/tenants/%s/create-operator"
                      (Id.value tenant.id)))
            ; a_method `Post
            ; a_class [ "stack" ]
            ]
          ((Component.csrf_element csrf ()
           :: CCList.map
                (fun (name, label) -> input_element `Text (Some name) label "")
                [ "email", Message.Email
                ; "password", Message.Password
                ; "firstname", Message.Firstname
                ; "lastname", Message.Lastname
                ])
          @ [ submit_element
                language
                Message.(Create (Some operator))
                ~classnames:[ "button--primary" ]
                ()
            ])
      ; a
          ~a:[ a_href (Sihl.Web.externalize_path "/root/tenants") ]
          [ txt "back" ]
      ]
  in
  Page_layout.create_root_layout html message language
;;
