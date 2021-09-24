open Tyxml.Html

let list csrf ~tenant_list ~message () =
  let open Tenant.Read in
  let build_tenant_rows tenant_list =
    CCList.map
      (fun tenant ->
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
  let tenant_list = build_tenant_rows tenant_list in
  let fields =
    [ "title", "Econ Uzh"
    ; "description", "adfasdf"
    ; "url", "pool.econ.uzh.ch"
    ; "database_url", "database@econ.uzh.ch"
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
    ; "email", "operator@econ.uzh.ch"
    ; "password", "adminadmin"
    ; "firstname", "Woofy"
    ; "lastname", "Woofer"
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
      ]
  in
  Page_layout.create ~children:html ~message ()
;;

let detail csrf ~tenant ~message () =
  let open Tenant.Read in
  let open Tenant.SmtpAuth.Read in
  let fields =
    [ "title", Tenant.Title.value tenant.title
    ; "description", Tenant.Description.value tenant.description
    ; "url", Tenant.Url.value tenant.url
    ; "database_url", ""
    ; "database_label", ""
    ; "smtp_auth_server", Tenant.SmtpAuth.Server.value tenant.smtp_auth.server
    ; "smtp_auth_port", Tenant.SmtpAuth.Port.value tenant.smtp_auth.port
    ; ( "smtp_auth_username"
      , Tenant.SmtpAuth.Username.value tenant.smtp_auth.username )
    ; "smtp_auth_password", ""
    ; ( "smtp_auth_authentication_method"
      , Tenant.SmtpAuth.AuthenticationMethod.value
          tenant.smtp_auth.authentication_method )
    ; ( "smtp_auth_protocol"
      , Tenant.SmtpAuth.Protocol.value tenant.smtp_auth.protocol )
    ; "styles", Tenant.Styles.value tenant.styles
    ; "icon", Tenant.Icon.value tenant.icon
    ; "logos", Tenant.Logos.value tenant.logos
    ; "partner_logos", Tenant.PartnerLogos.value tenant.partner_logos
    ; "default_language", Settings.Language.show tenant.default_language
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
      [ h1 [ txt (tenant.Tenant.Read.title |> Tenant.Title.value) ]
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
      ; a
          ~a:[ a_href (Sihl.Web.externalize_path "/root/tenants") ]
          [ txt "back" ]
      ]
  in
  Page_layout.create ~children:html ~message ()
;;
