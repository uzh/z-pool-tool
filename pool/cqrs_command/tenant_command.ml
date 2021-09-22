module Id = Pool_common.Id

module AddTenant : sig
  type t =
    { title : Tenant.Title.t
    ; description : Tenant.Description.t
    ; url : Tenant.Url.t
    ; database_url : Tenant.DatabaseUrl.t
    ; smtp_auth_server : Tenant.SmtpAuth.Server.t
    ; smtp_auth_port : Tenant.SmtpAuth.Port.t
    ; smtp_auth_username : Tenant.SmtpAuth.Username.t
    ; smtp_auth_authentication_method : Tenant.SmtpAuth.AuthenticationMethod.t
    ; smtp_auth_protocol : Tenant.SmtpAuth.Protocol.t
    ; styles : Tenant.Styles.t
    ; icon : Tenant.Icon.t
    ; logos : Tenant.Logos.t
    ; partner_logos : Tenant.PartnerLogos.t
    ; default_language : Settings.Language.t
    }

  val handle : t -> (Pool_event.t list, string) Result.t

  val decode
    :  (string * string list) list
    -> (t, Conformist.error list) Result.t

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { title : Tenant.Title.t
    ; description : Tenant.Description.t
    ; url : Tenant.Url.t
    ; database_url : Tenant.DatabaseUrl.t
    ; smtp_auth_server : Tenant.SmtpAuth.Server.t
    ; smtp_auth_port : Tenant.SmtpAuth.Port.t
    ; smtp_auth_username : Tenant.SmtpAuth.Username.t
    ; smtp_auth_authentication_method : Tenant.SmtpAuth.AuthenticationMethod.t
    ; smtp_auth_protocol : Tenant.SmtpAuth.Protocol.t
    ; styles : Tenant.Styles.t
    ; icon : Tenant.Icon.t
    ; logos : Tenant.Logos.t
    ; partner_logos : Tenant.PartnerLogos.t
    ; default_language : Settings.Language.t
    }

  let command
      title
      description
      url
      database_url
      smtp_auth_server
      smtp_auth_port
      smtp_auth_username
      smtp_auth_authentication_method
      smtp_auth_protocol
      styles
      icon
      logos
      partner_logos
      default_language
    =
    { title
    ; description
    ; url
    ; database_url
    ; smtp_auth_server
    ; smtp_auth_port
    ; smtp_auth_username
    ; smtp_auth_authentication_method
    ; smtp_auth_protocol
    ; styles
    ; icon
    ; logos
    ; partner_logos
    ; default_language
    }
  ;;

  let schema =
    Conformist.(
      make
        Field.
          [ Tenant.Title.schema ()
          ; Tenant.Description.schema ()
          ; Tenant.Url.schema ()
          ; Tenant.DatabaseUrl.schema ()
          ; Tenant.SmtpAuth.Server.schema ()
          ; Tenant.SmtpAuth.Port.schema ()
          ; Tenant.SmtpAuth.Username.schema ()
          ; Tenant.SmtpAuth.AuthenticationMethod.schema ()
          ; Tenant.SmtpAuth.Protocol.schema ()
          ; Tenant.Styles.schema ()
          ; Tenant.Icon.schema ()
          ; Tenant.Logos.schema ()
          ; Tenant.PartnerLogos.schema ()
          ; Settings.Language.schema ()
          ]
        command)
  ;;

  let handle (command : t) =
    let tenant =
      Tenant.
        { title = command.title
        ; description = command.description
        ; url = command.url
        ; database_url = command.database_url
        ; smtp_auth =
            SmtpAuth.
              { server = command.smtp_auth_server
              ; port = command.smtp_auth_port
              ; username = command.smtp_auth_username
              ; authentication_method = command.smtp_auth_authentication_method
              ; protocol = command.smtp_auth_protocol
              }
        ; styles = command.styles
        ; icon = command.icon
        ; logos = command.logos
        ; partner_logos = command.partner_logos
        ; default_language = command.default_language
        }
    in
    Ok [ Tenant.Added tenant |> Pool_event.tenant ]
  ;;

  let can user _ =
    Permission.can user ~any_of:[ Permission.Create Permission.Tenant ]
  ;;

  let decode data = Conformist.decode_and_validate schema data
end

module EditTenant : sig
  type t =
    { title : Tenant.Title.t
    ; description : Tenant.Description.t
    ; url : Tenant.Url.t
    ; database_url : Tenant.DatabaseUrl.t
    ; smtp_auth_server : Tenant.SmtpAuth.Server.t
    ; smtp_auth_port : Tenant.SmtpAuth.Port.t
    ; smtp_auth_username : Tenant.SmtpAuth.Username.t
    ; smtp_auth_authentication_method : Tenant.SmtpAuth.AuthenticationMethod.t
    ; smtp_auth_protocol : Tenant.SmtpAuth.Protocol.t
    ; styles : Tenant.Styles.t
    ; icon : Tenant.Icon.t
    ; logos : Tenant.Logos.t
    ; partner_logos : Tenant.PartnerLogos.t
    ; disabled : Tenant.Disabled.t
    ; default_language : Settings.Language.t
    }

  val handle : t -> Tenant.t -> (Pool_event.t list, string) result

  val decode
    :  (string * string list) list
    -> (t, Conformist.error list) Result.t

  val can : Sihl_user.t -> Tenant.t -> bool Lwt.t
end = struct
  type t =
    { title : Tenant.Title.t
    ; description : Tenant.Description.t
    ; url : Tenant.Url.t
    ; database_url : Tenant.DatabaseUrl.t
    ; smtp_auth_server : Tenant.SmtpAuth.Server.t
    ; smtp_auth_port : Tenant.SmtpAuth.Port.t
    ; smtp_auth_username : Tenant.SmtpAuth.Username.t
    ; smtp_auth_authentication_method : Tenant.SmtpAuth.AuthenticationMethod.t
    ; smtp_auth_protocol : Tenant.SmtpAuth.Protocol.t
    ; styles : Tenant.Styles.t
    ; icon : Tenant.Icon.t
    ; logos : Tenant.Logos.t
    ; partner_logos : Tenant.PartnerLogos.t
    ; disabled : Tenant.Disabled.t
    ; default_language : Settings.Language.t
    }

  let command
      title
      description
      url
      database_url
      smtp_auth_server
      smtp_auth_port
      smtp_auth_username
      smtp_auth_authentication_method
      smtp_auth_protocol
      styles
      icon
      logos
      partner_logos
      disabled
      default_language
    =
    { title
    ; description
    ; url
    ; database_url
    ; smtp_auth_server
    ; smtp_auth_port
    ; smtp_auth_username
    ; smtp_auth_authentication_method
    ; smtp_auth_protocol
    ; styles
    ; icon
    ; logos
    ; partner_logos
    ; disabled
    ; default_language
    }
  ;;

  let schema =
    Conformist.(
      make
        Field.
          [ Tenant.Title.schema ()
          ; Tenant.Description.schema ()
          ; Tenant.Url.schema ()
          ; Tenant.DatabaseUrl.schema ()
          ; Tenant.SmtpAuth.Server.schema ()
          ; Tenant.SmtpAuth.Port.schema ()
          ; Tenant.SmtpAuth.Username.schema ()
          ; Tenant.SmtpAuth.AuthenticationMethod.schema ()
          ; Tenant.SmtpAuth.Protocol.schema ()
          ; Tenant.Styles.schema ()
          ; Tenant.Icon.schema ()
          ; Tenant.Logos.schema ()
          ; Tenant.PartnerLogos.schema ()
          ; Tenant.Disabled.schema ()
          ; Settings.Language.schema ()
          ]
        command)
  ;;

  let handle (command : t) (tenant : Tenant.t) =
    let update =
      Tenant.
        { title = command.title
        ; description = command.description
        ; url = command.url
        ; database_url = command.database_url
        ; smtp_auth =
            SmtpAuth.
              { server = command.smtp_auth_server
              ; port = command.smtp_auth_port
              ; username = command.smtp_auth_username
              ; authentication_method = command.smtp_auth_authentication_method
              ; protocol = command.smtp_auth_protocol
              }
        ; styles = command.styles
        ; icon = command.icon
        ; logos = command.logos
        ; partner_logos = command.partner_logos
        ; disabled = command.disabled
        ; default_language = command.default_language
        }
    in
    Ok [ Tenant.Edited (tenant, update) |> Pool_event.tenant ]
  ;;

  let decode data = Conformist.decode_and_validate schema data

  let can user (tenant : Tenant.t) =
    Permission.can
      user
      ~any_of:[ Permission.Update (Permission.Tenant, Some tenant.Tenant.id) ]
  ;;
end

module DestroyTenant : sig
  type t = { tenant_id : string }

  val handle : t -> (Pool_event.t list, 'a) result
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
            (Permission.Tenant, Some (command.tenant_id |> Id.of_string))
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
    :  Tenant.t
    -> Admin.operator Admin.t
    -> (Pool_event.t list, string) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { user_id : Id.t
    ; tenant_id : Id.t
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
            (Permission.Tenant, Some (command.tenant_id |> Id.of_string))
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
