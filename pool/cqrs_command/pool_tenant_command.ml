module Conformist = Pool_common.Utils.PoolConformist
module Id = Pool_common.Id
module User = Pool_user
module File = Pool_common.File

let create_logo_mappings files tenant logo_type =
  let open Pool_tenant in
  CCList.map
    (fun asset_id ->
      LogoMapping.Write.
        { id = Id.create (); tenant_id = tenant.Write.id; asset_id; logo_type })
    files
;;

module Create : sig
  type t =
    { title : Pool_tenant.Title.t
    ; description : Pool_tenant.Description.t
    ; url : Pool_tenant.Url.t
    ; database_url : Pool_database.Url.t
    ; database_label : Pool_database.Label.t
    ; smtp_auth_server : Pool_tenant.SmtpAuth.Server.t
    ; smtp_auth_port : Pool_tenant.SmtpAuth.Port.t
    ; smtp_auth_username : Pool_tenant.SmtpAuth.Username.t
    ; smtp_auth_password : Pool_tenant.SmtpAuth.Password.t
    ; smtp_auth_authentication_method :
        Pool_tenant.SmtpAuth.AuthenticationMethod.t
    ; smtp_auth_protocol : Pool_tenant.SmtpAuth.Protocol.t
    ; styles : Pool_tenant.Styles.Write.t
    ; icon : Pool_tenant.Icon.Write.t
    ; default_language : Pool_common.Language.t
    ; tenant_logos : Id.t list
    ; partner_logos : Id.t list
    }

  val handle : t -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects : Ocauth.Authorizer.effect list
end = struct
  type t =
    { title : Pool_tenant.Title.t
    ; description : Pool_tenant.Description.t
    ; url : Pool_tenant.Url.t
    ; database_url : Pool_database.Url.t
    ; database_label : Pool_database.Label.t
    ; smtp_auth_server : Pool_tenant.SmtpAuth.Server.t
    ; smtp_auth_port : Pool_tenant.SmtpAuth.Port.t
    ; smtp_auth_username : Pool_tenant.SmtpAuth.Username.t
    ; smtp_auth_password : Pool_tenant.SmtpAuth.Password.t
    ; smtp_auth_authentication_method :
        Pool_tenant.SmtpAuth.AuthenticationMethod.t
    ; smtp_auth_protocol : Pool_tenant.SmtpAuth.Protocol.t
    ; styles : Pool_tenant.Styles.Write.t
    ; icon : Pool_tenant.Icon.Write.t
    ; default_language : Pool_common.Language.t
    ; tenant_logos : Id.t list
    ; partner_logos : Id.t list
    }

  let command
    title
    description
    url
    database_url
    database_label
    smtp_auth_server
    smtp_auth_port
    smtp_auth_username
    smtp_auth_password
    smtp_auth_authentication_method
    smtp_auth_protocol
    styles
    icon
    default_language
    tenant_logos
    partner_logos
    =
    { title
    ; description
    ; url
    ; database_url
    ; database_label
    ; smtp_auth_server
    ; smtp_auth_port
    ; smtp_auth_username
    ; smtp_auth_password
    ; smtp_auth_authentication_method
    ; smtp_auth_protocol
    ; styles
    ; icon
    ; default_language
    ; tenant_logos
    ; partner_logos
    }
  ;;

  let schema =
    Conformist.(
      make
        Field.
          [ Pool_tenant.Title.schema ()
          ; Pool_tenant.Description.schema ()
          ; Pool_tenant.Url.schema ()
          ; Pool_database.Url.schema ()
          ; Pool_database.Label.schema ()
          ; Pool_tenant.SmtpAuth.Server.schema ()
          ; Pool_tenant.SmtpAuth.Port.schema ()
          ; Pool_tenant.SmtpAuth.Username.schema ()
          ; Pool_tenant.SmtpAuth.Password.schema ()
          ; Pool_tenant.SmtpAuth.AuthenticationMethod.schema ()
          ; Pool_tenant.SmtpAuth.Protocol.schema ()
          ; Pool_tenant.Styles.Write.schema ()
          ; Pool_tenant.Icon.Write.schema ()
          ; Pool_common.Language.schema ()
          ; Pool_tenant.Logos.schema ()
          ; Pool_tenant.PartnerLogos.schema ()
          ]
        command)
  ;;

  let handle (command : t) =
    let database =
      Pool_database.
        { url = command.database_url; label = command.database_label }
    in
    let tenant =
      Pool_tenant.Write.create
        command.title
        command.description
        command.url
        database
        Pool_tenant.SmtpAuth.Write.
          { server = command.smtp_auth_server
          ; port = command.smtp_auth_port
          ; username = command.smtp_auth_username
          ; password = command.smtp_auth_password
          ; authentication_method = command.smtp_auth_authentication_method
          ; protocol = command.smtp_auth_protocol
          }
        command.styles
        command.icon
        command.default_language
    in
    let logo_mappings =
      CCList.map
        (fun (id_list, logo_type) ->
          create_logo_mappings id_list tenant logo_type)
        [ command.partner_logos, Pool_tenant.LogoMapping.LogoType.PartnerLogo
        ; command.tenant_logos, Pool_tenant.LogoMapping.LogoType.TenantLogo
        ]
      |> CCList.flatten
    in
    Ok
      [ Pool_tenant.Created tenant |> Pool_event.pool_tenant
      ; Pool_tenant.LogosUploaded logo_mappings |> Pool_event.pool_tenant
      ; Database.Added database |> Pool_event.database
      ; Database.Migrated command.database_label |> Pool_event.database
      ; Settings.(DefaultRestored default_values) |> Pool_event.settings
      ; I18n.(DefaultRestored default_values) |> Pool_event.i18n
      ; Email.(DefaultRestored default_values_tenant |> Pool_event.email)
      ]
  ;;

  let effects = [ `Create, `Role `Tenant ]

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;
end

module EditDetails : sig
  type t =
    { title : Pool_tenant.Title.t
    ; description : Pool_tenant.Description.t
    ; url : Pool_tenant.Url.t
    ; smtp_auth_server : Pool_tenant.SmtpAuth.Server.t
    ; smtp_auth_port : Pool_tenant.SmtpAuth.Port.t
    ; smtp_auth_username : Pool_tenant.SmtpAuth.Username.t
    ; smtp_auth_authentication_method :
        Pool_tenant.SmtpAuth.AuthenticationMethod.t
    ; smtp_auth_protocol : Pool_tenant.SmtpAuth.Protocol.t
    ; disabled : Pool_tenant.Disabled.t
    ; default_language : Pool_common.Language.t
    ; tenant_logos : Id.t list option
    ; partner_logos : Id.t list option
    }

  val handle
    :  Pool_tenant.Write.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects : Pool_tenant.Write.t -> Ocauth.Authorizer.effect list
end = struct
  type t =
    { title : Pool_tenant.Title.t
    ; description : Pool_tenant.Description.t
    ; url : Pool_tenant.Url.t
    ; smtp_auth_server : Pool_tenant.SmtpAuth.Server.t
    ; smtp_auth_port : Pool_tenant.SmtpAuth.Port.t
    ; smtp_auth_username : Pool_tenant.SmtpAuth.Username.t
    ; smtp_auth_authentication_method :
        Pool_tenant.SmtpAuth.AuthenticationMethod.t
    ; smtp_auth_protocol : Pool_tenant.SmtpAuth.Protocol.t
    ; disabled : Pool_tenant.Disabled.t
    ; default_language : Pool_common.Language.t
    ; tenant_logos : Id.t list option
    ; partner_logos : Id.t list option
    }

  let command
    title
    description
    url
    smtp_auth_server
    smtp_auth_port
    smtp_auth_username
    smtp_auth_authentication_method
    smtp_auth_protocol
    disabled
    default_language
    tenant_logos
    partner_logos
    =
    { title
    ; description
    ; url
    ; smtp_auth_server
    ; smtp_auth_port
    ; smtp_auth_username
    ; smtp_auth_authentication_method
    ; smtp_auth_protocol
    ; disabled
    ; default_language
    ; tenant_logos
    ; partner_logos
    }
  ;;

  let schema =
    Conformist.(
      make
        Field.
          [ Pool_tenant.Title.schema ()
          ; Pool_tenant.Description.schema ()
          ; Pool_tenant.Url.schema ()
          ; Pool_tenant.SmtpAuth.Server.schema ()
          ; Pool_tenant.SmtpAuth.Port.schema ()
          ; Pool_tenant.SmtpAuth.Username.schema ()
          ; Pool_tenant.SmtpAuth.AuthenticationMethod.schema ()
          ; Pool_tenant.SmtpAuth.Protocol.schema ()
          ; Pool_tenant.Disabled.schema ()
          ; Pool_common.Language.schema ()
          ; Conformist.optional @@ Pool_tenant.Logos.schema ()
          ; Conformist.optional @@ Pool_tenant.PartnerLogos.schema ()
          ]
        command)
  ;;

  let handle (tenant : Pool_tenant.Write.t) (command : t) =
    let update =
      Pool_tenant.
        { title = command.title
        ; description = command.description
        ; url = command.url
        ; smtp_auth =
            { server = command.smtp_auth_server
            ; port = command.smtp_auth_port
            ; username = command.smtp_auth_username
            ; authentication_method = command.smtp_auth_authentication_method
            ; protocol = command.smtp_auth_protocol
            }
        ; disabled = command.disabled
        ; default_language = command.default_language
        }
    in
    let logo_mappings =
      CCList.map
        (fun (id_list, logo_type) ->
          id_list
          |> CCOption.map (fun ids -> create_logo_mappings ids tenant logo_type)
          |> CCOption.value ~default:[])
        [ command.partner_logos, Pool_tenant.LogoMapping.LogoType.PartnerLogo
        ; command.tenant_logos, Pool_tenant.LogoMapping.LogoType.TenantLogo
        ]
      |> CCList.flatten
    in
    Ok
      [ Pool_tenant.DetailsEdited (tenant, update) |> Pool_event.pool_tenant
      ; Pool_tenant.LogosUploaded logo_mappings |> Pool_event.pool_tenant
      ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects { Pool_tenant.Write.id; _ } = [ `Update, `Uniq (Id.to_uuidm id) ]
end

module EditDatabase : sig
  type t =
    { database_url : Pool_database.Url.t
    ; database_label : Pool_database.Label.t
    }

  val handle
    :  Pool_tenant.Write.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects
    :  Pool_database.Label.t
    -> (Ocauth.Authorizer.effect list, Pool_common.Message.error) Lwt_result.t
end = struct
  type t =
    { database_url : Pool_database.Url.t
    ; database_label : Pool_database.Label.t
    }

  let command database_url database_label = { database_url; database_label }

  let schema =
    Conformist.(
      make
        Field.[ Pool_database.Url.schema (); Pool_database.Label.schema () ]
        command)
  ;;

  let handle (tenant : Pool_tenant.Write.t) (command : t) =
    let database =
      Pool_database.
        { url = command.database_url; label = command.database_label }
    in
    Ok
      [ Pool_tenant.DatabaseEdited (tenant, database) |> Pool_event.pool_tenant
      ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects dblabel =
    let open Lwt_result.Syntax in
    let* tenant = Pool_tenant.find_by_label dblabel in
    Lwt.return_ok [ `Update, `Uniq (Id.to_uuidm tenant.Pool_tenant.id) ]
  ;;
end

module DestroyLogo : sig
  val handle
    :  Pool_tenant.t
    -> Id.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Ocauth.Authorizer.effect list
end = struct
  let handle tenant asset_id =
    Ok [ Pool_tenant.LogoDeleted (tenant, asset_id) |> Pool_event.pool_tenant ]
  ;;

  let effects = [ `Create, `Role `Tenant ]
end

module Destroy : sig
  type t = { tenant_id : string }

  val handle : t -> (Pool_event.t list, Pool_common.Message.error) result
  val effects : t -> Ocauth.Authorizer.effect list
end = struct
  type t = { tenant_id : string }

  let handle t =
    Ok
      [ Pool_tenant.Destroyed (t.tenant_id |> Id.of_string)
        |> Pool_event.pool_tenant
      ]
  ;;

  let effects { tenant_id } =
    [ `Delete, `Uniq (Ocauth.Uuid.of_string_exn tenant_id) ]
  ;;
end
