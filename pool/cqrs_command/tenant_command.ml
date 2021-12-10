module Id = Pool_common.Id
module Database = Pool_common.Database
module User = Common_user
module File = Pool_common.File
module Url = Pool_common.Url

let create_logo_mappings files tenant logo_type =
  let open Tenant in
  CCList.map
    (fun asset_id ->
      LogoMapping.Write.
        { id = Id.create (); tenant_id = tenant.Write.id; asset_id; logo_type })
    files
;;

module Create : sig
  type t =
    { title : Tenant.Title.t
    ; description : Tenant.Description.t
    ; url : Url.t
    ; database_url : Database.Url.t
    ; database_label : Database.Label.t
    ; smtp_auth_server : Tenant.SmtpAuth.Server.t
    ; smtp_auth_port : Tenant.SmtpAuth.Port.t
    ; smtp_auth_username : Tenant.SmtpAuth.Username.t
    ; smtp_auth_password : Tenant.SmtpAuth.Password.t
    ; smtp_auth_authentication_method : Tenant.SmtpAuth.AuthenticationMethod.t
    ; smtp_auth_protocol : Tenant.SmtpAuth.Protocol.t
    ; styles : Tenant.Styles.Write.t
    ; icon : Tenant.Icon.Write.t
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
    { title : Tenant.Title.t
    ; description : Tenant.Description.t
    ; url : Url.t
    ; database_url : Database.Url.t
    ; database_label : Database.Label.t
    ; smtp_auth_server : Tenant.SmtpAuth.Server.t
    ; smtp_auth_port : Tenant.SmtpAuth.Port.t
    ; smtp_auth_username : Tenant.SmtpAuth.Username.t
    ; smtp_auth_password : Tenant.SmtpAuth.Password.t
    ; smtp_auth_authentication_method : Tenant.SmtpAuth.AuthenticationMethod.t
    ; smtp_auth_protocol : Tenant.SmtpAuth.Protocol.t
    ; styles : Tenant.Styles.Write.t
    ; icon : Tenant.Icon.Write.t
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
          [ Tenant.Title.schema ()
          ; Tenant.Description.schema ()
          ; Url.schema ()
          ; Database.Url.schema ()
          ; Database.Label.schema ()
          ; Tenant.SmtpAuth.Server.schema ()
          ; Tenant.SmtpAuth.Port.schema ()
          ; Tenant.SmtpAuth.Username.schema ()
          ; Tenant.SmtpAuth.Password.schema ()
          ; Tenant.SmtpAuth.AuthenticationMethod.schema ()
          ; Tenant.SmtpAuth.Protocol.schema ()
          ; Tenant.Styles.Write.schema ()
          ; Tenant.Icon.Write.schema ()
          ; Pool_common.Language.schema ()
          ; Tenant.Logos.schema ()
          ; Tenant.PartnerLogos.schema ()
          ]
        command)
  ;;

  let handle (command : t) =
    let open Tenant in
    let tenant =
      Tenant.Write.create
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
        [ command.partner_logos, Tenant.LogoMapping.LogoType.PartnerLogo
        ; command.tenant_logos, Tenant.LogoMapping.LogoType.TenantLogo
        ]
      |> CCList.flatten
    in
    Ok
      [ Tenant.Created tenant |> Pool_event.tenant
      ; Tenant.LogosUploaded logo_mappings |> Pool_event.tenant
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
    { title : Tenant.Title.t
    ; description : Tenant.Description.t
    ; url : Url.t
    ; smtp_auth_server : Tenant.SmtpAuth.Server.t
    ; smtp_auth_port : Tenant.SmtpAuth.Port.t
    ; smtp_auth_username : Tenant.SmtpAuth.Username.t
    ; smtp_auth_authentication_method : Tenant.SmtpAuth.AuthenticationMethod.t
    ; smtp_auth_protocol : Tenant.SmtpAuth.Protocol.t
    ; disabled : Tenant.Disabled.t
    ; default_language : Pool_common.Language.t
    ; tenant_logos : Id.t list option
    ; partner_logos : Id.t list option
    }

  val handle
    :  Tenant.Write.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val can : Sihl_user.t -> Tenant.t -> bool Lwt.t
end = struct
  type t =
    { title : Tenant.Title.t
    ; description : Tenant.Description.t
    ; url : Url.t
    ; smtp_auth_server : Tenant.SmtpAuth.Server.t
    ; smtp_auth_port : Tenant.SmtpAuth.Port.t
    ; smtp_auth_username : Tenant.SmtpAuth.Username.t
    ; smtp_auth_authentication_method : Tenant.SmtpAuth.AuthenticationMethod.t
    ; smtp_auth_protocol : Tenant.SmtpAuth.Protocol.t
    ; disabled : Tenant.Disabled.t
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
          [ Tenant.Title.schema ()
          ; Tenant.Description.schema ()
          ; Pool_common.Url.schema ()
          ; Tenant.SmtpAuth.Server.schema ()
          ; Tenant.SmtpAuth.Port.schema ()
          ; Tenant.SmtpAuth.Username.schema ()
          ; Tenant.SmtpAuth.AuthenticationMethod.schema ()
          ; Tenant.SmtpAuth.Protocol.schema ()
          ; Tenant.Disabled.schema ()
          ; Pool_common.Language.schema ()
          ; Conformist.optional @@ Tenant.Logos.schema ()
          ; Conformist.optional @@ Tenant.PartnerLogos.schema ()
          ]
        command)
  ;;

  let handle (tenant : Tenant.Write.t) (command : t) =
    let update =
      Tenant.
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
        [ command.partner_logos, Tenant.LogoMapping.LogoType.PartnerLogo
        ; command.tenant_logos, Tenant.LogoMapping.LogoType.TenantLogo
        ]
      |> CCList.flatten
    in
    Ok
      [ Tenant.DetailsEdited (tenant, update) |> Pool_event.tenant
      ; Tenant.LogosUploaded logo_mappings |> Pool_event.tenant
      ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.conformist
  ;;

  let can user (tenant : Tenant.t) =
    Permission.can
      user
      ~any_of:[ Permission.Update (Permission.Tenant, Some tenant.Tenant.id) ]
  ;;
end

module EditDatabase : sig
  type t =
    { database_url : Database.Url.t
    ; database_label : Database.Label.t
    }

  val handle
    :  Tenant.Write.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val can : Sihl_user.t -> Tenant.t -> bool Lwt.t
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

  let handle (tenant : Tenant.Write.t) (command : t) =
    let database =
      Database.{ url = command.database_url; label = command.database_label }
    in
    Ok [ Tenant.DatabaseEdited (tenant, database) |> Pool_event.tenant ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.conformist
  ;;

  let can user (tenant : Tenant.t) =
    Permission.can
      user
      ~any_of:[ Permission.Update (Permission.Tenant, Some tenant.Tenant.id) ]
  ;;
end

module DestroyLogo : sig
  val handle
    :  Tenant.t
    -> Id.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val can : Sihl_user.t -> bool Lwt.t
end = struct
  let handle tenant asset_id =
    Ok [ Tenant.LogoDeleted (tenant, asset_id) |> Pool_event.tenant ]
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
    Ok [ Tenant.Destroyed (t.tenant_id |> Id.of_string) |> Pool_event.tenant ]
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

module AssignOperator : sig
  type t =
    { user_id : Id.t
    ; tenant_id : Id.t
    }

  val handle
    :  Id.t
    -> Admin.operator Admin.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { user_id : Id.t
    ; tenant_id : Id.t
    }

  let handle tenant_id user =
    Ok [ Tenant.OperatorAssigned (tenant_id, user) |> Pool_event.tenant ]
  ;;

  let can user command =
    Permission.can
      user
      ~any_of:
        [ Permission.Manage (Permission.System, None)
        ; Permission.Manage (Permission.Tenant, Some command.tenant_id)
        ]
  ;;
end

module DivestOperator : sig
  type t =
    { user_id : string
    ; tenant_id : string
    }

  val handle
    :  Id.t
    -> Admin.operator Admin.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { user_id : string
    ; tenant_id : string
    }

  let handle tenant_id user =
    Ok [ Tenant.OperatorDivested (tenant_id, user) |> Pool_event.tenant ]
  ;;

  let can user command =
    Permission.can
      user
      ~any_of:
        [ Permission.Manage (Permission.System, None)
        ; Permission.Manage
            (Permission.Tenant, Some (command.tenant_id |> Id.of_string))
        ]
  ;;
end

module GenerateStatusReport : sig
  type t = { tenant_id : string }

  val handle
    :  t
    -> Tenant.t
    -> (Pool_event.t list, Pool_common.Message.error) result
end = struct
  type t = { tenant_id : string }

  let handle = Utils.todo
end

module AddRoot : sig
  type t = { user_id : string }

  val handle
    :  t
    -> Sihl_user.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = { user_id : string }

  let handle = Utils.todo

  let can user _ =
    Permission.can user ~any_of:[ Permission.Manage (Permission.System, None) ]
  ;;
end
