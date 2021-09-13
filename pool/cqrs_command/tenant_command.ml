open CCResult.Infix
open Tenant

module AddTenant : sig
  type t =
    { title : Title.t
    ; description : Description.t
    ; url : Url.t
    ; database : Database.t
    ; smtp_server : SmtpAuth.Server.t
    ; smtp_port : SmtpAuth.Port.t
    ; smtp_username : SmtpAuth.Protocol.t
    ; smtp_authentication_method : SmtpAuth.AuthenticationMethod.t
    ; smtp_protocol : SmtpAuth.Protocol.t
    ; styles : Styles.t
    ; icon : Icon.t
    ; logos : Logos.t
    ; partner_logos : PartnerLogo.t
    ; default_language : Settings.Language.t
    }

  val handle : t -> (Pool_event.t list, string) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { title : Title.t
    ; description : Description.t
    ; url : Url.t
    ; database : Database.t
    ; smtp_server : SmtpAuth.Server.t
    ; smtp_port : SmtpAuth.Port.t
    ; smtp_username : SmtpAuth.Protocol.t
    ; smtp_authentication_method : SmtpAuth.AuthenticationMethod.t
    ; smtp_protocol : SmtpAuth.Protocol.t
    ; styles : Styles.t
    ; icon : Icon.t
    ; logos : Logos.t
    ; partner_logos : PartnerLogo.t
    ; default_language : Settings.Language.t
    }

  let handle (command : t) =
    let create =
      let* smtp_auth =
        SmtpAuth.create
          command.smtp_server
          command.smtp_port
          command.smtp_username
          command.smtp_authentication_method
          command.smtp_protocol
          ()
      in
      Ok
        { title = command.title
        ; description = command.description
        ; url = command.url
        ; database = command.database
        ; smtp_auth
        ; styles = command.styles
        ; icon = command.icon
        ; logos = command.logos
        ; partner_logos = command.partner_logos
        ; default_language = command.default_language
        }
    in
    let event (t : Tenant.create) = [ Tenant.Added t |> Pool_event.tenant ] in
    create >|= event
  ;;

  let can user _ =
    Permission.can user ~any_of:[ Permission.Create Permission.Tenant ]
  ;;
end

module EditTenant : sig
  type t =
    { id : Common.Id.t
    ; title : Title.t
    ; description : Description.t
    ; url : Url.t
    ; database : Database.t
    ; smtp_server : SmtpAuth.Server.t
    ; smtp_port : SmtpAuth.Port.t
    ; smtp_username : SmtpAuth.Protocol.t
    ; smtp_authentication_method : SmtpAuth.AuthenticationMethod.t
    ; smtp_protocol : SmtpAuth.Protocol.t
    ; styles : Styles.t
    ; icon : Icon.t
    ; logos : Logos.t
    ; partner_logos : PartnerLogo.t
    ; disabled : Disabled.t
    ; default_language : Settings.Language.t
    }

  val handle : t -> Tenant.t -> (Pool_event.t list, string) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { id : Common.Id.t
    ; title : Title.t
    ; description : Description.t
    ; url : Url.t
    ; database : Database.t
    ; smtp_server : SmtpAuth.Server.t
    ; smtp_port : SmtpAuth.Port.t
    ; smtp_username : SmtpAuth.Protocol.t
    ; smtp_authentication_method : SmtpAuth.AuthenticationMethod.t
    ; smtp_protocol : SmtpAuth.Protocol.t
    ; styles : Styles.t
    ; icon : Icon.t
    ; logos : Logos.t
    ; partner_logos : PartnerLogo.t
    ; disabled : Disabled.t
    ; default_language : Settings.Language.t
    }

  let handle (command : t) (tenant : Tenant.t) =
    let update =
      let* smtp_auth =
        SmtpAuth.create
          command.smtp_server
          command.smtp_port
          command.smtp_username
          command.smtp_authentication_method
          command.smtp_protocol
          ()
      in
      Ok
        { title = command.title
        ; description = command.description
        ; url = command.url
        ; database = command.database
        ; smtp_auth
        ; styles = command.styles
        ; icon = command.icon
        ; logos = command.logos
        ; partner_logos = command.partner_logos
        ; disabled = command.disabled
        ; default_language = command.default_language
        }
    in
    let event (t : Tenant.update) =
      [ Tenant.Edited (tenant, t) |> Pool_event.tenant ]
    in
    update >|= event
  ;;

  let can user command =
    Permission.can
      user
      ~any_of:[ Permission.Update (Permission.Tenant, Some command.id) ]
  ;;
end

module DestroyTenant : sig
  type t = { tenant_id : string }

  val handle : t -> (Pool_event.t list, 'a) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = { tenant_id : string }

  let handle t =
    Ok
      [ Tenant.Destroyed (t.tenant_id |> Common.Id.of_string)
        |> Pool_event.tenant
      ]
  ;;

  let can user command =
    Permission.can
      user
      ~any_of:
        [ Permission.Destroy
            (Permission.Tenant, Some (command.tenant_id |> Common.Id.of_string))
        ]
  ;;
end

module DisableTenant : sig
  type t = { tenant_id : string }

  val handle : Tenant.t -> (Pool_event.t list, 'b) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = { tenant_id : string }

  let handle tenant = Ok [ Tenant.Disabled tenant |> Pool_event.tenant ]

  let can user command =
    Permission.can
      user
      ~any_of:
        [ Permission.Destroy
            (Permission.Tenant, Some (command.tenant_id |> Common.Id.of_string))
        ]
  ;;
end

module EnableTenant : sig
  type t = { tenant_id : string }

  val handle : Tenant.t -> (Pool_event.t list, 'b) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = { tenant_id : string }

  let handle tenant = Ok [ Tenant.Enabled tenant |> Pool_event.tenant ]

  let can user command =
    Permission.can
      user
      ~any_of:
        [ Permission.Destroy
            (Permission.Tenant, Some (command.tenant_id |> Common.Id.of_string))
        ]
  ;;
end

module AssignOperator : sig
  type t =
    { user_id : Common.Id.t
    ; tenant_id : Common.Id.t
    }

  val handle
    :  Tenant.t
    -> Admin.operator Admin.t
    -> (Pool_event.t list, string) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { user_id : Common.Id.t
    ; tenant_id : Common.Id.t
    }

  let handle tenant user =
    Ok [ Tenant.OperatorAssigned (tenant, user) |> Pool_event.tenant ]
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
    :  Tenant.t
    -> Admin.operator Admin.t
    -> (Pool_event.t list, string) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { user_id : string
    ; tenant_id : string
    }

  let handle tenant user =
    Ok [ Tenant.OperatorDivested (tenant, user) |> Pool_event.tenant ]
  ;;

  let can user command =
    Permission.can
      user
      ~any_of:
        [ Permission.Manage (Permission.System, None)
        ; Permission.Manage
            (Permission.Tenant, Some (command.tenant_id |> Common.Id.of_string))
        ]
  ;;
end

module GenerateStatusReport : sig
  type t = { tenant_id : string }

  val handle : t -> Tenant.t -> (Pool_event.t list, string) result
end = struct
  type t = { tenant_id : string }

  let handle = Utils.todo
end

module AddRoot : sig
  type t = { user_id : string }

  val handle : t -> Sihl_user.t -> (Pool_event.t list, string) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = { user_id : string }

  let handle = Utils.todo

  let can user _ =
    Permission.can user ~any_of:[ Permission.Manage (Permission.System, None) ]
  ;;
end
