module Tenant_command = Cqrs_command.Tenant_command
module Tenant_pool_command = Cqrs_command.Tenant_pool_command
module Admin_command = Cqrs_command.Admin_command
module HttpUtils = Http_utils
module Common = Pool_common

module Data = struct
  module Asset = struct
    open Database.SeedAssets

    let styles = dummy_css () |> fun { id; _ } -> id
    let icon = dummy_icon () |> fun { id; _ } -> id
    let tenant_logo = dummy_tenant_logo () |> fun { id; _ } -> id
    let partner_logo = dummy_partner_logo () |> fun { id; _ } -> id
  end

  let title = "Econ uzh"
  let description = "description"
  let url = "pool.econ.uzh.ch"
  let database_url = "mariadb://root@database-tenant:3306/test_econ"
  let database_label = "econ-test"
  let smtp_auth_server = "smtp.uzh.ch"
  let smtp_auth_port = "587"
  let smtp_auth_username = "engineering@econ.uzh.ch"
  let smtp_auth_password = "emailemail"
  let smtp_auth_authentication_method = "LOGIN"
  let smtp_auth_protocol = "STARTTLS"

  let styles =
    Asset.styles
    |> Tenant_pool.Styles.Write.create
    |> Test_utils.get_or_failwith_pool_error
  ;;

  let icon =
    Asset.icon
    |> Tenant_pool.Icon.Write.create
    |> Test_utils.get_or_failwith_pool_error
  ;;

  let tenant_logo = Asset.tenant_logo
  let partner_logo = Asset.partner_logo
  let default_language = "EN"
  let email = "operator@econ.uzh.ch"
  let password = "adminadmin"
  let firstname = "DJ"
  let lastname = "Ötzi"

  let urlencoded =
    let open Tenant_pool.LogoMapping.LogoType in
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
    ; "styles", [ Asset.styles ]
    ; "icon", [ Asset.icon ]
    ; to_string TenantLogo, [ tenant_logo ]
    ; to_string PartnerLogo, [ partner_logo ]
    ; "default_language", [ default_language ]
    ; "email", [ email ]
    ; "password", [ password ]
    ; "firstname", [ firstname ]
    ; "lastname", [ lastname ]
    ]
  ;;

  let tenant =
    let open Tenant_pool in
    let open CCResult in
    let* title = title |> Title.create in
    let* description = description |> Description.create in
    let* url = url |> Tenant_pool.Url.create in
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
    let* database = Database_pool.create database_url database_label in
    Ok
      Write.
        { id = Common.Id.create ()
        ; title
        ; description
        ; url
        ; database
        ; smtp_auth
        ; styles
        ; icon
        ; maintenance = Maintenance.create false
        ; disabled = Disabled.create false
        ; default_language = Common.Language.En
        ; created_at = Common.CreatedAt.create ()
        ; updated_at = Common.UpdatedAt.create ()
        }
  ;;
end

let create_smtp_auth () =
  let open Data in
  let open Tenant_pool.SmtpAuth in
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
  let expected = Error Common.Message.(Invalid SmtpProtocol) in
  Alcotest.(
    check
      (result Test_utils.tenant_smtp_auth Test_utils.error)
      "succeeds"
      expected
      smtp_auth)
;;

let[@warning "-4"] create_tenant () =
  let open Data in
  let events =
    let open CCResult.Infix in
    Tenant_pool_command.Create.(Data.urlencoded |> decode >>= handle)
  in
  let ( tenant_id
      , created_at
      , updated_at
      , (logo_id, logo_asset_id)
      , (partner_logo_id, partner_logo_asset_id) )
    =
    (* Read Ids and timestamps to create an equal event list *)
    events
    |> Test_utils.get_or_failwith_pool_error
    |> function
    | [ Pool_event.TenantPool
          Tenant_pool.(Created Write.{ id; created_at; updated_at; _ })
      ; Pool_event.TenantPool
          (Tenant_pool.LogosUploaded [ partner_logo; tenant_logo ])
      ] ->
      let read_ids Tenant_pool.LogoMapping.Write.{ id; asset_id; _ } =
        id, asset_id
      in
      ( id
      , created_at
      , updated_at
      , tenant_logo |> read_ids
      , partner_logo |> read_ids )
    | _ -> failwith "Tenant create events don't match in test."
  in
  let expected =
    let open Tenant_pool in
    let open CCResult in
    let* title = title |> Tenant_pool.Title.create in
    let* description = description |> Tenant_pool.Description.create in
    let* url = url |> Tenant_pool.Url.create in
    let* database =
      let open Database_pool in
      let* url = database_url |> Url.create in
      let* label = database_label |> Label.create in
      Ok { url; label }
    in
    let* smtp_auth =
      let open Tenant_pool.SmtpAuth in
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
    let* default_language = default_language |> Common.Language.of_string in
    let create : Tenant_pool.Write.t =
      Tenant_pool.Write.
        { id = tenant_id
        ; title
        ; description
        ; url
        ; database
        ; smtp_auth
        ; styles
        ; icon
        ; maintenance = Maintenance.create false
        ; disabled = Disabled.create false
        ; default_language
        ; created_at
        ; updated_at
        }
    in
    let logos : Tenant_pool.LogoMapping.Write.t list =
      Tenant_pool.LogoMapping.Write.
        [ { id = partner_logo_id
          ; tenant_id
          ; asset_id = partner_logo_asset_id
          ; logo_type = Tenant_pool.LogoMapping.LogoType.PartnerLogo
          }
        ; { id = logo_id
          ; tenant_id
          ; asset_id = logo_asset_id
          ; logo_type = Tenant_pool.LogoMapping.LogoType.TenantLogo
          }
        ]
    in
    Ok
      [ Tenant_pool.Created create |> Pool_event.tenant_pool
      ; Tenant_pool.LogosUploaded logos |> Pool_event.tenant_pool
      ]
  in
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      events)
;;

let[@warning "-4"] update_tenant_details () =
  let open Data in
  match Data.tenant with
  | Error _ -> failwith "Failed to create tenant"
  | Ok tenant ->
    let events =
      let open CCResult.Infix in
      let open Tenant_pool_command.EditDetails in
      Data.urlencoded
      |> HttpUtils.format_request_boolean_values [ "disabled" ]
      |> decode
      >>= handle tenant
    in
    let expected =
      let open Tenant_pool in
      let open CCResult in
      let* title = title |> Title.create in
      let* description = description |> Description.create in
      let* url = url |> Tenant_pool.Url.create in
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
      let* default_language = default_language |> Common.Language.of_string in
      let disabled = false |> Disabled.create in
      let update : update =
        { title; description; url; smtp_auth; default_language; disabled }
      in
      let logo_event =
        (* read logo event, as it's not value of update in this test *)
        events
        |> Test_utils.get_or_failwith_pool_error
        |> function
        | [ _
          ; (Pool_event.TenantPool (Tenant_pool.LogosUploaded [ _; _ ]) as
            logos)
          ] -> logos
        | _ -> failwith "Tenant create events don't match in test."
      in
      Ok
        [ DetailsEdited (tenant, update) |> Pool_event.tenant_pool; logo_event ]
    in
    Alcotest.(
      check
        (result (list Test_utils.event) Test_utils.error)
        "succeeds"
        expected
        events)
;;

let update_tenant_database () =
  let open Data in
  match Data.tenant with
  | Error _ -> failwith "Failed to create tenant"
  | Ok tenant ->
    let events =
      let open CCResult.Infix in
      let open Tenant_pool_command.EditDatabase in
      [ "database_url", [ database_url ]; "database_label", [ database_label ] ]
      |> decode
      >>= handle tenant
    in
    let expected =
      let open Database_pool in
      let open CCResult in
      let* url = database_url |> Url.create in
      let* label = database_label |> Label.create in
      let database = { url; label } in
      Ok
        [ Tenant_pool.DatabaseEdited (tenant, database)
          |> Pool_event.tenant_pool
        ]
    in
    Alcotest.(
      check
        (result (list Test_utils.event) Test_utils.error)
        "succeeds"
        expected
        events)
;;

let create_operator () =
  let open Data in
  match Data.tenant with
  | Error _ -> failwith "Failed to create tenant"
  | Ok tenant ->
    let events =
      let open CCResult.Infix in
      let open Admin_command.CreateOperator in
      Data.urlencoded |> decode >>= handle tenant
    in
    let expected =
      let open CCResult in
      let* email = email |> Common_user.EmailAddress.create in
      let* password = password |> Common_user.Password.create in
      let* firstname = firstname |> Common_user.Firstname.create in
      let* lastname = lastname |> Common_user.Lastname.create in
      let operator : Admin.create =
        Admin.{ email; password; firstname; lastname }
      in
      Ok [ Admin.Created (Admin.Operator, operator) |> Pool_event.admin ]
    in
    Alcotest.(
      check
        (result (list Test_utils.event) Test_utils.error)
        "succeeds"
        expected
        events)
;;
