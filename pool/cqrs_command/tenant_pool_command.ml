module Id = Pool_common.Id
module Database = Pool_common.Database
module User = Common_user
module File = Pool_common.File
module Url = Pool_common.Url

let create_logo_mappings files tenant logo_type =
  let open Tenant_pool in
  CCList.map
    (fun asset_id ->
      LogoMapping.Write.
        { id = Id.create (); tenant_id = tenant.Write.id; asset_id; logo_type })
    files
;;

module Create : sig
  type t =
    { title : Tenant_pool.Title.t
    ; description : Tenant_pool.Description.t
    ; url : Url.t
    ; database_url : Database.Url.t
    ; database_label : Database.Label.t
    ; smtp_auth_server : Tenant_pool.SmtpAuth.Server.t
    ; smtp_auth_port : Tenant_pool.SmtpAuth.Port.t
    ; smtp_auth_username : Tenant_pool.SmtpAuth.Username.t
    ; smtp_auth_password : Tenant_pool.SmtpAuth.Password.t
    ; smtp_auth_authentication_method :
        Tenant_pool.SmtpAuth.AuthenticationMethod.t
    ; smtp_auth_protocol : Tenant_pool.SmtpAuth.Protocol.t
    ; styles : Tenant_pool.Styles.Write.t
    ; icon : Tenant_pool.Icon.Write.t
    ; default_language : Pool_common.Language.t
    ; tenant_logos : Id.t list
    ; partner_logos : Id.t list
    }

  val handle : t -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { title : Tenant_pool.Title.t
    ; description : Tenant_pool.Description.t
    ; url : Url.t
    ; database_url : Database.Url.t
    ; database_label : Database.Label.t
    ; smtp_auth_server : Tenant_pool.SmtpAuth.Server.t
    ; smtp_auth_port : Tenant_pool.SmtpAuth.Port.t
    ; smtp_auth_username : Tenant_pool.SmtpAuth.Username.t
    ; smtp_auth_password : Tenant_pool.SmtpAuth.Password.t
    ; smtp_auth_authentication_method :
        Tenant_pool.SmtpAuth.AuthenticationMethod.t
    ; smtp_auth_protocol : Tenant_pool.SmtpAuth.Protocol.t
    ; styles : Tenant_pool.Styles.Write.t
    ; icon : Tenant_pool.Icon.Write.t
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
          [ Tenant_pool.Title.schema ()
          ; Tenant_pool.Description.schema ()
          ; Url.schema ()
          ; Database.Url.schema ()
          ; Database.Label.schema ()
          ; Tenant_pool.SmtpAuth.Server.schema ()
          ; Tenant_pool.SmtpAuth.Port.schema ()
          ; Tenant_pool.SmtpAuth.Username.schema ()
          ; Tenant_pool.SmtpAuth.Password.schema ()
          ; Tenant_pool.SmtpAuth.AuthenticationMethod.schema ()
          ; Tenant_pool.SmtpAuth.Protocol.schema ()
          ; Tenant_pool.Styles.Write.schema ()
          ; Tenant_pool.Icon.Write.schema ()
          ; Pool_common.Language.schema ()
          ; Tenant_pool.Logos.schema ()
          ; Tenant_pool.PartnerLogos.schema ()
          ]
        command)
  ;;

  let handle (command : t) =
    let open Tenant_pool in
    let tenant =
      Tenant_pool.Write.create
        command.title
        command.description
        command.url
        Database.{ url = command.database_url; label = command.database_label }
        SmtpAuth.Write.
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
        [ command.partner_logos, Tenant_pool.LogoMapping.LogoType.PartnerLogo
        ; command.tenant_logos, Tenant_pool.LogoMapping.LogoType.TenantLogo
        ]
      |> CCList.flatten
    in
    Ok
      [ Tenant_pool.Created tenant |> Pool_event.tenant_pool
      ; Tenant_pool.LogosUploaded logo_mappings |> Pool_event.tenant_pool
      ]
  ;;

  let can user _ =
    Permission.can user ~any_of:[ Permission.Create Permission.Tenant ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.conformist
  ;;
end

module EditDetails : sig
  type t =
    { title : Tenant_pool.Title.t
    ; description : Tenant_pool.Description.t
    ; url : Url.t
    ; smtp_auth_server : Tenant_pool.SmtpAuth.Server.t
    ; smtp_auth_port : Tenant_pool.SmtpAuth.Port.t
    ; smtp_auth_username : Tenant_pool.SmtpAuth.Username.t
    ; smtp_auth_authentication_method :
        Tenant_pool.SmtpAuth.AuthenticationMethod.t
    ; smtp_auth_protocol : Tenant_pool.SmtpAuth.Protocol.t
    ; disabled : Tenant_pool.Disabled.t
    ; default_language : Pool_common.Language.t
    ; tenant_logos : Id.t list option
    ; partner_logos : Id.t list option
    }

  val handle
    :  Tenant_pool.Write.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val can : Sihl_user.t -> Tenant_pool.t -> bool Lwt.t
end = struct
  type t =
    { title : Tenant_pool.Title.t
    ; description : Tenant_pool.Description.t
    ; url : Url.t
    ; smtp_auth_server : Tenant_pool.SmtpAuth.Server.t
    ; smtp_auth_port : Tenant_pool.SmtpAuth.Port.t
    ; smtp_auth_username : Tenant_pool.SmtpAuth.Username.t
    ; smtp_auth_authentication_method :
        Tenant_pool.SmtpAuth.AuthenticationMethod.t
    ; smtp_auth_protocol : Tenant_pool.SmtpAuth.Protocol.t
    ; disabled : Tenant_pool.Disabled.t
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
          [ Tenant_pool.Title.schema ()
          ; Tenant_pool.Description.schema ()
          ; Pool_common.Url.schema ()
          ; Tenant_pool.SmtpAuth.Server.schema ()
          ; Tenant_pool.SmtpAuth.Port.schema ()
          ; Tenant_pool.SmtpAuth.Username.schema ()
          ; Tenant_pool.SmtpAuth.AuthenticationMethod.schema ()
          ; Tenant_pool.SmtpAuth.Protocol.schema ()
          ; Tenant_pool.Disabled.schema ()
          ; Pool_common.Language.schema ()
          ; Conformist.optional @@ Tenant_pool.Logos.schema ()
          ; Conformist.optional @@ Tenant_pool.PartnerLogos.schema ()
          ]
        command)
  ;;

  let handle (tenant : Tenant_pool.Write.t) (command : t) =
    let update =
      Tenant_pool.
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
        [ command.partner_logos, Tenant_pool.LogoMapping.LogoType.PartnerLogo
        ; command.tenant_logos, Tenant_pool.LogoMapping.LogoType.TenantLogo
        ]
      |> CCList.flatten
    in
    Ok
      [ Tenant_pool.DetailsEdited (tenant, update) |> Pool_event.tenant_pool
      ; Tenant_pool.LogosUploaded logo_mappings |> Pool_event.tenant_pool
      ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.conformist
  ;;

  let can user (tenant : Tenant_pool.t) =
    Permission.can
      user
      ~any_of:
        [ Permission.Update (Permission.Tenant, Some tenant.Tenant_pool.id) ]
  ;;
end

module EditDatabase : sig
  type t =
    { database_url : Database.Url.t
    ; database_label : Database.Label.t
    }

  val handle
    :  Tenant_pool.Write.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val can : Sihl_user.t -> Tenant_pool.t -> bool Lwt.t
end = struct
  type t =
    { database_url : Database.Url.t
    ; database_label : Database.Label.t
    }

  let command database_url database_label = { database_url; database_label }

  let schema =
    Conformist.(
      make Field.[ Database.Url.schema (); Database.Label.schema () ] command)
  ;;

  let handle (tenant : Tenant_pool.Write.t) (command : t) =
    let database =
      Database.{ url = command.database_url; label = command.database_label }
    in
    Ok
      [ Tenant_pool.DatabaseEdited (tenant, database) |> Pool_event.tenant_pool
      ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.conformist
  ;;

  let can user (tenant : Tenant_pool.t) =
    Permission.can
      user
      ~any_of:
        [ Permission.Update (Permission.Tenant, Some tenant.Tenant_pool.id) ]
  ;;
end

module DestroyLogo : sig
  val handle
    :  Tenant_pool.t
    -> Id.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val can : Sihl_user.t -> bool Lwt.t
end = struct
  let handle tenant asset_id =
    Ok [ Tenant_pool.LogoDeleted (tenant, asset_id) |> Pool_event.tenant_pool ]
  ;;

  let can user =
    Permission.can user ~any_of:[ Permission.Create Permission.Tenant ]
  ;;
end

module Destroy : sig
  type t = { tenant_id : string }

  val handle : t -> (Pool_event.t list, Pool_common.Message.error) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = { tenant_id : string }

  let handle t =
    Ok
      [ Tenant_pool.Destroyed (t.tenant_id |> Id.of_string)
        |> Pool_event.tenant_pool
      ]
  ;;

  let can user command =
    Permission.can
      user
      ~any_of:
        [ Permission.Destroy
            (Permission.Tenant, Some (command.tenant_id |> Id.of_string))
        ]
  ;;
end
