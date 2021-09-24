open Tyxml.Html

let build_tenant_rows tenant_list =
  CCList.map
    (fun (tenant : Tenant.Read.t) ->
      div [ h2 [ txt (tenant.Tenant.Read.title |> Tenant.Title.value) ]; hr () ])
    tenant_list
;;

let list csrf ~tenant_list ~message () =
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
