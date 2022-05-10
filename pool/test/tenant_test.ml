module Tenant_command = Cqrs_command.Tenant_command
module Pool_tenant_command = Cqrs_command.Pool_tenant_command
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
    |> Pool_tenant.Styles.Write.create
    |> Test_utils.get_or_failwith_pool_error
  ;;

  let icon =
    Asset.icon
    |> Pool_tenant.Icon.Write.create
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
    let open Common.Message in
    [ Field.Title, [ title ]
    ; Field.Description, [ description ]
    ; Field.Url, [ url ]
    ; Field.DatabaseUrl, [ database_url ]
    ; Field.DatabaseLabel, [ database_label ]
    ; Field.SmtpAuthServer, [ smtp_auth_server ]
    ; Field.SmtpPort, [ smtp_auth_port ]
    ; Field.SmtpUsername, [ smtp_auth_username ]
    ; Field.SmtpPassword, [ smtp_auth_password ]
    ; Field.SmtpAuthMethod, [ smtp_auth_authentication_method ]
    ; Field.SmtpProtocol, [ smtp_auth_protocol ]
    ; Field.Styles, [ Asset.styles ]
    ; Field.Icon, [ Asset.icon ]
    ; Field.TenantLogos, [ tenant_logo ]
    ; Field.PartnerLogos, [ partner_logo ]
    ; Field.Language, [ default_language ]
    ; Field.Email, [ email ]
    ; Field.Password, [ password ]
    ; Field.Firstname, [ firstname ]
    ; Field.Lastname, [ lastname ]
    ]
    |> CCList.map (CCPair.map_fst Field.show)
  ;;

  let tenant =
    let open Pool_tenant in
    let open CCResult in
    let* title = title |> Title.create in
    let* description = description |> Description.create in
    let* url = url |> Pool_tenant.Url.create in
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
    let* database = Pool_database.create database_url database_label in
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
  let open Pool_tenant.SmtpAuth in
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
  let expected = Error Common.Message.(Invalid Field.SmtpProtocol) in
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
    Pool_tenant_command.Create.(Data.urlencoded |> decode >>= handle)
  in
  let ( tenant_id
      , created_at
      , updated_at
      , (logo_id, logo_asset_id)
      , (partner_logo_id, partner_logo_asset_id)
      , database_label )
    =
    (* Read Ids and timestamps to create an equal event list *)
    events
    |> Test_utils.get_or_failwith_pool_error
    |> function
    | [ Pool_event.PoolTenant
          Pool_tenant.(Created Write.{ id; created_at; updated_at; _ })
      ; Pool_event.PoolTenant
          (Pool_tenant.LogosUploaded [ partner_logo; tenant_logo ])
      ; Pool_event.Database (Database.Added _)
      ; Pool_event.Database (Database.Migrated database_label)
      ; Pool_event.Settings (Settings.DefaultRestored _)
      ; Pool_event.I18n (I18n.DefaultRestored _)
      ; Pool_event.EmailAddress (Email.DefaultRestored _)
      ] ->
      let read_ids Pool_tenant.LogoMapping.Write.{ id; asset_id; _ } =
        id, asset_id
      in
      ( id
      , created_at
      , updated_at
      , tenant_logo |> read_ids
      , partner_logo |> read_ids
      , database_label )
    | _ -> failwith "Tenant create events don't match in test."
  in
  let expected =
    let open CCResult in
    let* title = title |> Pool_tenant.Title.create in
    let* description = description |> Pool_tenant.Description.create in
    let* url = url |> Pool_tenant.Url.create in
    let* (database : Pool_database.t) =
      let* url = database_url |> Pool_tenant.Database.Url.create in
      Ok Pool_database.{ url; label = database_label }
    in
    let* smtp_auth =
      let open Pool_tenant.SmtpAuth in
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
    let create : Pool_tenant.Write.t =
      Pool_tenant.Write.
        { id = tenant_id
        ; title
        ; description
        ; url
        ; database
        ; smtp_auth
        ; styles
        ; icon
        ; maintenance = Pool_tenant.Maintenance.create false
        ; disabled = Pool_tenant.Disabled.create false
        ; default_language
        ; created_at
        ; updated_at
        }
    in
    let logos : Pool_tenant.LogoMapping.Write.t list =
      Pool_tenant.LogoMapping.Write.
        [ { id = partner_logo_id
          ; tenant_id
          ; asset_id = partner_logo_asset_id
          ; logo_type = Pool_tenant.LogoMapping.LogoType.PartnerLogo
          }
        ; { id = logo_id
          ; tenant_id
          ; asset_id = logo_asset_id
          ; logo_type = Pool_tenant.LogoMapping.LogoType.TenantLogo
          }
        ]
    in
    Ok
      [ Pool_tenant.Created create |> Pool_event.pool_tenant
      ; Pool_tenant.LogosUploaded logos |> Pool_event.pool_tenant
      ; Database.Added database |> Pool_event.database
      ; Database.Migrated database_label |> Pool_event.database
      ; Settings.(DefaultRestored default_values) |> Pool_event.settings
      ; I18n.(DefaultRestored default_values) |> Pool_event.i18n
      ; Email.(DefaultRestored default_values_tenant)
        |> Pool_event.email_address
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
      let open Pool_tenant_command.EditDetails in
      Data.urlencoded
      |> HttpUtils.format_request_boolean_values
           [ Common.Message.Field.(TenantDisabledFlag |> show) ]
      |> decode
      >>= handle tenant
    in
    let expected =
      let open Pool_tenant in
      let open CCResult in
      let* title = title |> Title.create in
      let* description = description |> Description.create in
      let* url = url |> Pool_tenant.Url.create in
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
          ; (Pool_event.PoolTenant (Pool_tenant.LogosUploaded [ _; _ ]) as
            logos)
          ] -> logos
        | _ -> failwith "Tenant create events don't match in test."
      in
      Ok
        [ DetailsEdited (tenant, update) |> Pool_event.pool_tenant; logo_event ]
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
      let open Pool_tenant_command.EditDatabase in
      Common.Message.Field.
        [ DatabaseUrl |> show, [ database_url ]
        ; DatabaseLabel |> show, [ database_label ]
        ]
      |> decode
      >>= handle tenant
    in
    let expected =
      let open Pool_database in
      let open CCResult in
      let* url = database_url |> Url.create in
      let* label = database_label |> Label.create in
      let database = { url; label } in
      Ok
        [ Pool_tenant.DatabaseEdited (tenant, database)
          |> Pool_event.pool_tenant
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
  let events =
    let open CCResult.Infix in
    let open Admin_command.CreateOperator in
    Data.urlencoded |> decode >>= handle
  in
  let expected =
    let open CCResult in
    let* email = email |> Pool_user.EmailAddress.create in
    let* password = password |> Pool_user.Password.create in
    let* firstname = firstname |> Pool_user.Firstname.create in
    let* lastname = lastname |> Pool_user.Lastname.create in
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
