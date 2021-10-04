open Tyxml.Html

let list csrf tenant_list root_list message () =
  let open Tenant in
  let build_tenant_rows tenant_list =
    CCList.map
      (fun (tenant : Tenant.t) ->
        div
          [ h2 [ txt (tenant.title |> Tenant.Title.value) ]
          ; a
              ~a:
                [ a_href
                    (Sihl.Web.externalize_path
                       (Format.asprintf
                          "/root/tenant/%s"
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
        [ input ~a:[ a_input_type `Submit; a_value text ] () ]
    in
    CCList.map
      (fun (root : Admin.root Admin.t) ->
        let user = root |> Admin.user in
        let status = status_toggle user.status user.id in
        div [ h2 [ txt user.email ]; status; hr () ])
      root_list
  in
  let tenant_list = build_tenant_rows tenant_list in
  let root_list = build_root_rows root_list in
  let fields =
    [ "title", "Econ Uzh"
    ; "description", "adfasdf"
    ; "url", "pool.econ.uzh.ch"
    ; "database_url", "mariadb://root@database:3306/dev_econ"
    ; "database_label", "econ_uzh"
    ; "smtp_auth_server", "smtp.econ.uzh.ch"
    ; "smtp_auth_port", "587"
    ; "smtp_auth_username", "engineering@econ.uzh.ch"
    ; "smtp_auth_password", "emailemail"
    ; "smtp_auth_authentication_method", "LOGIN"
    ; "smtp_auth_protocol", "SSL/TLS"
    ; "styles", "custom-styles.css"
    ; "icon", "icon"
    ; "logos", "logos"
    ; "partner_logos", "partner_logos"
    ; "default_language", "DE"
    ]
  in
  let input_fields =
    CCList.map
      (fun (name, value) ->
        input
          ~a:
            [ a_input_type `Text
            ; a_name name
            ; a_placeholder name
            ; a_value value
            ]
          ())
      fields
  in
  let html =
    div
      [ h1 [ txt "Tenants" ]
      ; div tenant_list
      ; form
          ~a:
            [ a_action (Sihl.Web.externalize_path "/root/tenant/create")
            ; a_method `Post
            ]
          (CCList.concat
             [ [ Component.csrf_element csrf () ]
             ; input_fields
             ; [ input ~a:[ a_input_type `Submit; a_value "Create new" ] () ]
             ])
      ; hr ()
      ; h1 [ txt "Root users" ]
      ; div root_list
      ; form
          ~a:[ a_action (Format.asprintf "/root/root/create"); a_method `Post ]
          (CCList.concat
             [ CCList.map
                 (fun field ->
                   input ~a:[ a_name field; a_placeholder field ] ())
                 [ "email"; "password"; "firstname"; "lastname" ]
             ; [ input ~a:[ a_input_type `Submit; a_value "Create root" ] () ]
             ])
      ]
  in
  Page_layout.create html message ()
;;

let detail csrf (tenant : Tenant.t) message () =
  let open Tenant in
  let open Tenant.SmtpAuth in
  let detail_fields =
    [ "title", Tenant.Title.value tenant.title
    ; "description", Tenant.Description.value tenant.description
    ; "url", Tenant.Url.value tenant.url
    ; "smtp_auth_server", Tenant.SmtpAuth.Server.value tenant.smtp_auth.server
    ; "smtp_auth_port", Tenant.SmtpAuth.Port.value tenant.smtp_auth.port
    ; ( "smtp_auth_username"
      , Tenant.SmtpAuth.Username.value tenant.smtp_auth.username )
    ; ( "smtp_auth_authentication_method"
      , Tenant.SmtpAuth.AuthenticationMethod.value
          tenant.smtp_auth.authentication_method )
    ; ( "smtp_auth_protocol"
      , Tenant.SmtpAuth.Protocol.value tenant.smtp_auth.protocol )
    ; "styles", Tenant.Styles.value tenant.styles
    ; "icon", Tenant.Icon.value tenant.icon
    ; "logos", Tenant.Logos.value tenant.logos
    ; "partner_logos", Tenant.PartnerLogos.value tenant.partner_logos
    ; "default_language", Settings.Language.code tenant.default_language
    ]
  in
  let database_fields =
    [ "database_url", ""
    ; "database_label", Tenant.Database.Label.value tenant.database_label
    ]
  in
  let detail_input_fields =
    CCList.map
      (fun (name, value) ->
        input
          ~a:
            [ a_input_type `Text
            ; a_name name
            ; a_placeholder name
            ; a_value value
            ]
          ())
      detail_fields
  in
  let database_input_fields =
    CCList.map
      (fun (name, value) ->
        input
          ~a:
            [ a_input_type `Text
            ; a_name name
            ; a_placeholder name
            ; a_value value
            ]
          ())
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
  let html =
    div
      [ h1 [ txt (tenant.Tenant.title |> Tenant.Title.value) ]
      ; form
          ~a:
            [ a_action
                (Sihl.Web.externalize_path
                   (Format.asprintf
                      "/root/tenant/%s/update-detail"
                      (Pool_common.Id.value tenant.id)))
            ; a_method `Post
            ]
          (CCList.concat
             [ [ Component.csrf_element csrf () ]
             ; detail_input_fields
             ; [ disabled ]
             ; [ input ~a:[ a_input_type `Submit; a_value "Update" ] () ]
             ])
      ; hr ()
      ; form
          ~a:
            [ a_action
                (Sihl.Web.externalize_path
                   (Format.asprintf
                      "/root/tenant/%s/update-database"
                      (Pool_common.Id.value tenant.id)))
            ; a_method `Post
            ]
          (CCList.concat
             [ [ Component.csrf_element csrf () ]
             ; database_input_fields
             ; [ input ~a:[ a_input_type `Submit; a_value "Update database" ] ()
               ]
             ])
      ; hr ()
      ; form
          ~a:
            [ a_action
                (Format.asprintf
                   "/root/tenant/%s/create-operator"
                   (Pool_common.Id.value tenant.id))
            ; a_method `Post
            ]
          (CCList.concat
             [ CCList.map
                 (fun field ->
                   input ~a:[ a_name field; a_placeholder field ] ())
                 [ "email"; "password"; "firstname"; "lastname" ]
             ; [ input ~a:[ a_input_type `Submit; a_value "Create operator" ] ()
               ]
             ])
      ; a
          ~a:[ a_href (Sihl.Web.externalize_path "/root/tenants") ]
          [ txt "back" ]
      ]
  in
  Page_layout.create html message ()
;;
