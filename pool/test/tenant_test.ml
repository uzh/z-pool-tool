module Tenant_command = Cqrs_command.Tenant_command

let create_tenant () =
  let title = "Econ uzh" in
  let description = "description" in
  let url = "pool.econ.uzh.ch" in
  let database_url = "mariadb://root@database:3306/dev_econ" in
  let database_label = "econ-uzh" in
  let smtp_auth_server = "smtp.uzh.ch" in
  let smtp_auth_port = "587" in
  let smtp_auth_username = "engineering@econ.uzh.ch" in
  let smtp_auth_password = "emailemail" in
  let smtp_auth_authentication_method = "LOGIN" in
  let smtp_auth_protocol = "STARTTLS" in
  let styles = "custom-styles.econ.css" in
  let icon = "some icon" in
  let logos = "some logo" in
  let partner_logos = "some partner logos" in
  let default_language = "EN" in
  let email = "operator@econ.uzh.ch" in
  let password = "adminadmin" in
  let firstname = "DJ" in
  let lastname = "Ötzi" in
  let command =
    CCResult.get_exn
    @@ Tenant_command.Create.decode
         [ "title", [ title ]
         ; "description", [ description ]
         ; "url", [ url ]
         ; "database_url", [ database_url ]
         ; "database_label", [ database_label ]
         ; "smtp_auth_server", [ smtp_auth_server ]
         ; "smtp_auth_port", [ smtp_auth_port ]
         ; "smtp_auth_username", [ smtp_auth_username ]
         ; "smtp_auth_password", [ smtp_auth_password ]
         ; ( "smtp_auth_authentication_method"
           , [ smtp_auth_authentication_method ] )
         ; "smtp_auth_protocol", [ smtp_auth_protocol ]
         ; "styles", [ styles ]
         ; "icon", [ icon ]
         ; "logos", [ logos ]
         ; "partner_logos", [ partner_logos ]
         ; "default_language", [ default_language ]
         ; "email", [ email ]
         ; "password", [ password ]
         ; "firstname", [ firstname ]
         ; "lastname", [ lastname ]
         ]
  in
  let events = Tenant_command.Create.handle command in
  let expected =
    let open Tenant in
    let ( let* ) = Result.bind in
    let* title = title |> Tenant.Title.create in
    let* description = description |> Tenant.Description.create in
    let* url = url |> Tenant.Url.create in
    let* database =
      let open Tenant.Database in
      let* url = database_url |> Url.create in
      let* label = database_label |> Label.create in
      Ok { url; label }
    in
    let* smtp_auth =
      let open Tenant.SmtpAuth in
      let* server = smtp_auth_server |> Server.create in
      let* port = smtp_auth_port |> Port.create in
      let* username = smtp_auth_username |> Username.create in
      let* password = smtp_auth_password |> Password.create in
      let* authentication_method =
        smtp_auth_authentication_method |> AuthenticationMethod.create
      in
      let* protocol = smtp_auth_protocol |> Protocol.create in
      Ok
        Write.
          { server; port; username; password; authentication_method; protocol }
    in
    let* styles = styles |> Styles.create in
    let* icon = icon |> Icon.create in
    let* logos = logos |> Logos.create in
    let* partner_logos = partner_logos |> PartnerLogos.create in
    let* default_language = default_language |> Settings.Language.of_string in
    let create : Tenant.create =
      { title
      ; description
      ; url
      ; database
      ; smtp_auth
      ; styles
      ; icon
      ; logos
      ; partner_logos
      ; default_language
      }
    in
    Ok [ Tenant.Created create |> Pool_event.tenant ]
  in
  Alcotest.(
    check (result (list Test_utils.event) string) "succeeds" expected events)
;;

let create_tenant_invalid_smtp_port () =
  let title = "Econ uzh" in
  let description = "description" in
  let url = "pool.econ.uzh.ch" in
  let database_url = "mariadb://root@database:3306/dev_econ" in
  let database_label = "econ-uzh" in
  let smtp_auth_server = "smtp.uzh.ch" in
  let smtp_auth_port = "123" in
  let smtp_auth_username = "engineering@econ.uzh.ch" in
  let smtp_auth_password = "emailemail" in
  let smtp_auth_authentication_method = "LOGIN" in
  let smtp_auth_protocol = "STARTTLS" in
  let styles = "custom-styles.econ.css" in
  let icon = "some icon" in
  let logos = "some logo" in
  let partner_logos = "some partner logos" in
  let default_language = "EN" in
  let email = "operator@econ.uzh.ch" in
  let password = "adminadmin" in
  let firstname = "DJ" in
  let lastname = "Ötzi" in
  let events =
    let open CCResult.Infix in
    Tenant_command.Create.decode
      [ "title", [ title ]
      ; "description", [ description ]
      ; "url", [ url ]
      ; "database_url", [ database_url ]
      ; "database_label", [ database_label ]
      ; "smtp_auth_server", [ smtp_auth_server ]
      ; "smtp_auth_port", [ smtp_auth_port ]
      ; "smtp_auth_username", [ smtp_auth_username ]
      ; "smtp_auth_password", [ smtp_auth_password ]
      ; "smtp_auth_authentication_method", [ smtp_auth_authentication_method ]
      ; "smtp_auth_protocol", [ smtp_auth_protocol ]
      ; "styles", [ styles ]
      ; "icon", [ icon ]
      ; "logos", [ logos ]
      ; "partner_logos", [ partner_logos ]
      ; "default_language", [ default_language ]
      ; "email", [ email ]
      ; "password", [ password ]
      ; "firstname", [ firstname ]
      ; "lastname", [ lastname ]
      ]
    |> CCResult.map_err Utils.handle_conformist_error
    >>= Tenant_command.Create.handle
  in
  let expected = Error "smtp_auth_port: Invalid SMTP port!" in
  Alcotest.(
    check (result (list Test_utils.event) string) "succeeds" expected events)
;;
