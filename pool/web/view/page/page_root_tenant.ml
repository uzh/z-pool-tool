open Tyxml.Html
module HttpUtils = Http_utils
module File = Pool_common.File

let input_element = Component.input_element

let list csrf tenant_list root_list message () =
  let build_tenant_rows tenant_list =
    let open Tenant in
    CCList.map
      (fun (tenant : Tenant.t) ->
        div
          [ h2 [ txt (tenant.title |> Tenant.Title.value) ]
          ; a
              ~a:
                [ a_href
                    (Sihl.Web.externalize_path
                       (Format.asprintf
                          "/root/tenants/%s"
                          (Pool_common.Id.value tenant.id)))
                ]
              [ txt "detail" ]
          ; hr ()
          ])
      tenant_list
  in
  let build_root_rows root_list =
    let open Sihl.Contract.User in
    let status_toggle (status : Sihl.Contract.User.status) id =
      let text =
        match status with
        | Active -> "Disable"
        | Inactive -> "Enable"
      in
      form
        ~a:
          [ a_action
              (Sihl.Web.externalize_path
                 (Format.asprintf "/root/root/%s/toggle-status" id))
          ; a_method `Post
          ]
        [ input_element `Submit None text ]
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
  let fields =
    [ "title", "title"
    ; "description", "descr"
    ; "url", "url"
    ; "database_url", "db url"
    ; "database_label", "label"
    ; "smtp_auth_server", "server"
    ; "smtp_auth_port", "587"
    ; "smtp_auth_username", "username"
    ; "smtp_auth_password", "pw"
    ; "smtp_auth_authentication_method", "LOGIN"
    ; "smtp_auth_protocol", "SSL/TLS"
    ; "partner_logo", "partner logos"
    ; "default_language", "DE"
    ]
  in
  let input_fields =
    CCList.map
      (fun (name, value) -> input_element `Text (Some name) value)
      fields
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
              ~a:
                [ a_input_type `File
                ; a_name (Tenant.LogoMapping.stringify_type `TenantLogo)
                ; a_multiple ()
                ]
              ()
          ]
      ; div
          [ label [ txt "Add partner logos" ]
          ; input
              ~a:
                [ a_input_type `File
                ; a_name (Tenant.LogoMapping.stringify_type `PartnerLogo)
                ; a_multiple ()
                ]
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
            ]
          ((Component.csrf_element csrf () :: input_fields)
          @ [ input_element `Submit None "Create new" ])
      ; hr ()
      ; h1 [ txt "Root users" ]
      ; div root_list
      ; form
          ~a:[ a_action (Format.asprintf "/root/root/create"); a_method `Post ]
          (CCList.map
             (fun name -> input_element `Text (Some name) "")
             [ "email"; "password"; "firstname"; "lastname" ]
          @ [ input_element `Submit None "Create root" ])
      ]
  in
  Page_layout.create html message ()
;;

let detail csrf (tenant : Tenant.t) message () =
  let open Tenant in
  let open Tenant.SmtpAuth in
  let detail_fields =
    [ "title", Title.value tenant.title
    ; "description", Description.value tenant.description
    ; "url", Url.value tenant.url
    ; "smtp_auth_server", Server.value tenant.smtp_auth.server
    ; "smtp_auth_port", Port.value tenant.smtp_auth.port
    ; "smtp_auth_username", Username.value tenant.smtp_auth.username
    ; ( "smtp_auth_authentication_method"
      , AuthenticationMethod.value tenant.smtp_auth.authentication_method )
    ; "smtp_auth_protocol", Protocol.value tenant.smtp_auth.protocol
    ; "default_language", Settings.Language.code tenant.default_language
    ]
  in
  let database_fields =
    [ "database_url", ""
    ; "database_label", Pool_common.Database.Label.value tenant.database_label
    ]
  in
  let detail_input_fields =
    CCList.map
      (fun (name, value) -> input_element `Text (Some name) value)
      detail_fields
    @ [ div
          [ a
              ~a:[ a_href (File.path (tenant.styles |> Tenant.Styles.value)) ]
              [ txt "styles" ]
          ; input ~a:[ a_input_type `File; a_name "styles" ] ()
          ]
      ; div
          [ a
              ~a:[ a_href (File.path (tenant.icon |> Tenant.Icon.value)) ]
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
      (fun (name, value) -> input_element `Text (Some name) value)
      database_fields
  in
  let disabled =
    let attributes =
      match tenant.disabled |> Tenant.Disabled.value with
      | true -> [ a_input_type `Checkbox; a_name "disabled"; a_checked () ]
      | false -> [ a_input_type `Checkbox; a_name "disabled" ]
    in
    input ~a:attributes ()
  in
  let delete_img_form files =
    div
      ~a:[ a_style "display: flex;" ]
      (CCList.map
         (fun (file : Pool_common.File.t) ->
           div
             [ img
                 ~src:(File.path file)
                 ~alt:""
                 ~a:[ a_style "width: 200px" ]
                 ()
             ; form
                 ~a:
                   [ a_action
                       (Format.asprintf
                          "/root/tenants/%s/assets/%s/delete"
                          (tenant.id |> Pool_common.Id.value)
                          (Pool_common.File.id file |> Pool_common.Id.value))
                   ; a_method `Post
                   ]
                 [ Component.csrf_element csrf ()
                 ; input_element `Submit None "Delete Image"
                 ]
             ])
         files)
  in
  let delete_file_forms =
    div
      [ h3 [ txt "Tenant Logos" ]
      ; delete_img_form (tenant.logos |> Tenant.Logos.value)
      ; h3 [ txt "Partner Logos" ]
      ; delete_img_form (tenant.partner_logo |> Tenant.PartnerLogos.value)
      ]
  in
  let html =
    div
      [ h1 [ txt (tenant.Tenant.title |> Tenant.Title.value) ]
      ; form
          ~a:
            [ a_action
                (Sihl.Web.externalize_path
                   (Format.asprintf
                      "/root/tenants/%s/update-detail"
                      (Pool_common.Id.value tenant.id)))
            ; a_method `Post
            ; a_enctype "multipart/form-data"
            ]
          ((Component.csrf_element csrf () :: detail_input_fields)
          @ [ disabled; input_element `Submit None "Update" ])
      ; hr ()
      ; delete_file_forms
      ; hr ()
      ; form
          ~a:
            [ a_action
                (Sihl.Web.externalize_path
                   (Format.asprintf
                      "/root/tenants/%s/update-database"
                      (Pool_common.Id.value tenant.id)))
            ; a_method `Post
            ; a_enctype "multipart/form-data"
            ]
          ((Component.csrf_element csrf () :: database_input_fields)
          @ [ input_element `Submit None "Update database" ])
      ; hr ()
      ; form
          ~a:
            [ a_action
                (Format.asprintf
                   "/root/tenants/%s/create-operator"
                   (Pool_common.Id.value tenant.id))
            ; a_method `Post
            ]
          ((Component.csrf_element csrf ()
           :: CCList.map
                (fun name -> input_element `Text (Some name) "")
                [ "email"; "password"; "firstname"; "lastname" ])
          @ [ input_element `Submit None "Create operator" ])
      ; a
          ~a:[ a_href (Sihl.Web.externalize_path "/root/tenants") ]
          [ txt "back" ]
      ]
  in
  Page_layout.create html message ()
;;
