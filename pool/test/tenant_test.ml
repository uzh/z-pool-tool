module Tenant_command = Cqrs_command.Tenant_command
module Admin_command = Cqrs_command.Admin_command
module HttpUtils = Http_utils

module Data = struct
  let title = "Econ uzh"
  let description = "description"
  let url = "pool.econ.uzh.ch"
  let database_url = "mariadb://root@database:3306/dev_econ"
  let database_label = "econ-uzh"
  let smtp_auth_server = "smtp.uzh.ch"
  let smtp_auth_port = "587"
  let smtp_auth_username = "engineering@econ.uzh.ch"
  let smtp_auth_password = "emailemail"
  let smtp_auth_authentication_method = "LOGIN"
  let smtp_auth_protocol = "STARTTLS"
  let styles = "custom-styles.econ.css"
  let icon = "some icon"
  let logos = "some logo"
  let partner_logos = "some partner logos"
  let default_language = "EN"
  let email = "operator@econ.uzh.ch"
  let password = "adminadmin"
  let firstname = "DJ"
  let lastname = "Ötzi"
end

let create_smtp_auth () =
  let open Data in
  let open Tenant.SmtpAuth in
  let smtp_auth =
    let open CCResult in
    let* server = smtp_auth_server |> Server.create in
    let* port = smtp_auth_port |> Port.create in
    let* username = smtp_auth_username |> Username.create in
    let* authentication_method =
      smtp_auth_authentication_method |> AuthenticationMethod.create
    in
    let* protocol = "http" |> Protocol.create in
    Ok { server; port; username; authentication_method; protocol }
  in
  let expected = Error "Invalid SMTP protocol!" in
  Alcotest.(
    check
      (result Test_utils.tenant_smtp_auth string)
      "succeeds"
      expected
      smtp_auth)
;;

let create_tenant () =
  let open Data in
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
  let expected =
    let open Tenant in
    let open CCResult in
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

let update_tenant_details () =
  let open Tenant in
  let open Data in
  let tenant =
    let open CCResult in
    let* title = title |> Title.create in
    let* description = description |> Description.create in
    let* url = url |> Url.create in
    let* smtp_auth_server = smtp_auth_server |> SmtpAuth.Server.create in
    let* smtp_auth_port = smtp_auth_port |> SmtpAuth.Port.create in
    let* smtp_auth_username = smtp_auth_username |> SmtpAuth.Username.create in
    let* smtp_auth_password = smtp_auth_password |> SmtpAuth.Password.create in
    let* smtp_auth_authentication_method =
      smtp_auth_authentication_method |> SmtpAuth.AuthenticationMethod.create
    in
    let* smtp_auth_protocol = smtp_auth_protocol |> SmtpAuth.Protocol.create in
    let* smtp_auth =
      SmtpAuth.Write.create
        smtp_auth_server
        smtp_auth_port
        smtp_auth_username
        smtp_auth_password
        smtp_auth_authentication_method
        smtp_auth_protocol
    in
    let* database_label = database_label |> Database.Label.create in
    let* database_url = database_url |> Database.Url.create in
    let* database = Database.create database_url database_label in
    let* styles = styles |> Styles.create in
    let* icon = icon |> Icon.create in
    let* logos = logos |> Logos.create in
    let* partner_logos = partner_logos |> PartnerLogos.create in
    let disabled = false |> Disabled.create in
    let maintenance = false |> Maintenance.create in
    let* default_language = "EN" |> Settings.Language.of_string in
    Ok
      Write.
        { id = Pool_common.Id.create ()
        ; title
        ; description
        ; url
        ; database
        ; smtp_auth
        ; styles
        ; icon
        ; logos
        ; partner_logos
        ; maintenance
        ; disabled
        ; default_language
        ; created_at = Pool_common.CreatedAt.create ()
        ; updated_at = Pool_common.UpdatedAt.create ()
        }
  in
  match tenant with
  | Error _ -> failwith "Failed to create tenant"
  | Ok tenant ->
    let events =
      let open CCResult.Infix in
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
      |> HttpUtils.format_request_boolean_values [ "disabled" ]
      |> Tenant_command.EditDetails.decode
      |> CCResult.map_err Utils.handle_conformist_error
      >>= CCFun.flip Tenant_command.EditDetails.handle tenant
    in
    let expected =
      let open Tenant in
      let open CCResult in
      let* title = title |> Title.create in
      let* description = description |> Description.create in
      let* url = url |> Url.create in
      let* smtp_auth =
        let* server = smtp_auth_server |> SmtpAuth.Server.create in
        let* port = smtp_auth_port |> SmtpAuth.Port.create in
        let* username = smtp_auth_username |> SmtpAuth.Username.create in
        let* authentication_method =
          smtp_auth_authentication_method
          |> SmtpAuth.AuthenticationMethod.create
        in
        let* protocol = smtp_auth_protocol |> SmtpAuth.Protocol.create in
        Ok { server; port; username; authentication_method; protocol }
      in
      let* styles = styles |> Styles.create in
      let* icon = icon |> Icon.create in
      let* logos = logos |> Logos.create in
      let* partner_logos = partner_logos |> PartnerLogos.create in
      let* default_language = default_language |> Settings.Language.of_string in
      let disabled = false |> Disabled.create in
      let update : update =
        { title
        ; description
        ; url
        ; smtp_auth
        ; styles
        ; icon
        ; logos
        ; partner_logos
        ; default_language
        ; disabled
        }
      in
      Ok [ DetailsEdited (tenant, update) |> Pool_event.tenant ]
    in
    Alcotest.(
      check (result (list Test_utils.event) string) "succeeds" expected events)
;;

let update_tenant_database () =
  let open Data in
  let open Tenant in
  let tenant =
    let open CCResult in
    let* title = title |> Title.create in
    let* description = description |> Description.create in
    let* url = url |> Url.create in
    let* smtp_auth_server = smtp_auth_server |> SmtpAuth.Server.create in
    let* smtp_auth_port = smtp_auth_port |> SmtpAuth.Port.create in
    let* smtp_auth_username = smtp_auth_username |> SmtpAuth.Username.create in
    let* smtp_auth_password = smtp_auth_password |> SmtpAuth.Password.create in
    let* smtp_auth_authentication_method =
      smtp_auth_authentication_method |> SmtpAuth.AuthenticationMethod.create
    in
    let* smtp_auth_protocol = smtp_auth_protocol |> SmtpAuth.Protocol.create in
    let* smtp_auth =
      SmtpAuth.Write.create
        smtp_auth_server
        smtp_auth_port
        smtp_auth_username
        smtp_auth_password
        smtp_auth_authentication_method
        smtp_auth_protocol
    in
    let* database_label = database_label |> Database.Label.create in
    let* database_url = database_url |> Database.Url.create in
    let* database = Database.create database_url database_label in
    let* styles = styles |> Styles.create in
    let* icon = icon |> Icon.create in
    let* logos = logos |> Logos.create in
    let* partner_logos = partner_logos |> PartnerLogos.create in
    let disabled = false |> Disabled.create in
    let maintenance = false |> Maintenance.create in
    let* default_language = "EN" |> Settings.Language.of_string in
    Ok
      Write.
        { id = Pool_common.Id.create ()
        ; title
        ; description
        ; url
        ; database
        ; smtp_auth
        ; styles
        ; icon
        ; logos
        ; partner_logos
        ; maintenance
        ; disabled
        ; default_language
        ; created_at = Pool_common.CreatedAt.create ()
        ; updated_at = Pool_common.UpdatedAt.create ()
        }
  in
  match tenant with
  | Error _ -> failwith "Failed to create tenant"
  | Ok tenant ->
    let events =
      let open CCResult.Infix in
      [ "database_url", [ database_url ]; "database_label", [ database_label ] ]
      |> Tenant_command.EditDatabase.decode
      |> CCResult.map_err Utils.handle_conformist_error
      >>= CCFun.flip Tenant_command.EditDatabase.handle tenant
    in
    let expected =
      let open Tenant.Database in
      let open CCResult in
      let* url = database_url |> Url.create in
      let* label = database_label |> Label.create in
      let database = { url; label } in
      Ok [ Tenant.DatabaseEdited (tenant, database) |> Pool_event.tenant ]
    in
    Alcotest.(
      check (result (list Test_utils.event) string) "succeeds" expected events)
;;

let create_operator () =
  let open Data in
  let open Tenant in
  let tenant =
    let open CCResult in
    let* title = title |> Title.create in
    let* description = description |> Description.create in
    let* url = url |> Url.create in
    let* smtp_auth_server = smtp_auth_server |> SmtpAuth.Server.create in
    let* smtp_auth_port = smtp_auth_port |> SmtpAuth.Port.create in
    let* smtp_auth_username = smtp_auth_username |> SmtpAuth.Username.create in
    let* smtp_auth_password = smtp_auth_password |> SmtpAuth.Password.create in
    let* smtp_auth_authentication_method =
      smtp_auth_authentication_method |> SmtpAuth.AuthenticationMethod.create
    in
    let* smtp_auth_protocol = smtp_auth_protocol |> SmtpAuth.Protocol.create in
    let* smtp_auth =
      SmtpAuth.Write.create
        smtp_auth_server
        smtp_auth_port
        smtp_auth_username
        smtp_auth_password
        smtp_auth_authentication_method
        smtp_auth_protocol
    in
    let* database_label = database_label |> Database.Label.create in
    let* database_url = database_url |> Database.Url.create in
    let* database = Database.create database_url database_label in
    let* styles = styles |> Styles.create in
    let* icon = icon |> Icon.create in
    let* logos = logos |> Logos.create in
    let* partner_logos = partner_logos |> PartnerLogos.create in
    let disabled = false |> Disabled.create in
    let maintenance = false |> Maintenance.create in
    let* default_language = "EN" |> Settings.Language.of_string in
    Ok
      Write.
        { id = Pool_common.Id.create ()
        ; title
        ; description
        ; url
        ; database
        ; smtp_auth
        ; styles
        ; icon
        ; logos
        ; partner_logos
        ; maintenance
        ; disabled
        ; default_language
        ; created_at = Pool_common.CreatedAt.create ()
        ; updated_at = Pool_common.UpdatedAt.create ()
        }
  in
  match tenant with
  | Error _ -> failwith "Failed to create tenant"
  | Ok tenant ->
    let events =
      let open CCResult.Infix in
      [ "email", [ email ]
      ; "password", [ password ]
      ; "firstname", [ firstname ]
      ; "lastname", [ lastname ]
      ]
      |> Admin_command.CreateOperator.decode
      |> CCResult.map_err Utils.handle_conformist_error
      >>= CCFun.flip Admin_command.CreateOperator.handle tenant
    in
    let expected =
      let open CCResult in
      let* email = email |> Common_user.Email.Address.create in
      let* password = password |> Common_user.Password.create in
      let* firstname = firstname |> Common_user.Firstname.create in
      let* lastname = lastname |> Common_user.Lastname.create in
      let operator : Admin.create =
        Admin.{ email; password; firstname; lastname }
      in
      Ok [ Admin.Created (Admin.Operator, operator) |> Pool_event.admin ]
    in
    Alcotest.(
      check (result (list Test_utils.event) string) "succeeds" expected events)
;;
