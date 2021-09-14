open CCResult.Infix
open Tenant

module AddTenant : sig
  type t =
    { title : Title.t
    ; description : Description.t
    ; url : Url.t
    ; database : Database.t
    ; smtp_auth_server : SmtpAuth.Server.t
    ; smtp_auth_port : SmtpAuth.Port.t
    ; smtp_auth_username : SmtpAuth.Username.t
    ; smtp_auth_authentication_method : SmtpAuth.AuthenticationMethod.t
    ; smtp_auth_protocol : SmtpAuth.Protocol.t
    ; styles : Styles.t
    ; icon : Icon.t
    ; logos : Logos.t
    ; partner_logos : PartnerLogo.t
    ; default_language : Settings.Language.t
    }

  val handle : t -> (Pool_event.t list, string) result
  val decode : Conformist.input -> (create, Conformist.error list) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { title : Title.t
    ; description : Description.t
    ; url : Url.t
    ; database : Database.t
    ; smtp_auth_server : SmtpAuth.Server.t
    ; smtp_auth_port : SmtpAuth.Port.t
    ; smtp_auth_username : SmtpAuth.Username.t
    ; smtp_auth_authentication_method : SmtpAuth.AuthenticationMethod.t
    ; smtp_auth_protocol : SmtpAuth.Protocol.t
    ; styles : Styles.t
    ; icon : Icon.t
    ; logos : Logos.t
    ; partner_logos : PartnerLogo.t
    ; default_language : Settings.Language.t
    }

  let command
      title
      description
      url
      database
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
    ; database
    ; smtp_auth =
        { server = smtp_auth_server
        ; port = smtp_auth_port
        ; username = smtp_auth_username
        ; authentication_method = smtp_auth_authentication_method
        ; protocol = smtp_auth_protocol
        }
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
        [ custom
            (fun l -> l |> List.hd |> Title.create)
            (fun l -> [ Title.show l ])
            "title"
        ; custom
            (fun l -> l |> List.hd |> Description.create)
            (fun l -> [ Description.show l ])
            "description"
        ; custom
            (fun l -> l |> List.hd |> Url.create)
            (fun l -> [ Url.show l ])
            "url"
        ; custom
            (fun l -> l |> List.hd |> Database.create)
            (fun l -> [ Database.show l ])
            "daetabase"
        ; custom
            (fun l -> l |> List.hd |> SmtpAuth.Server.create)
            (fun l -> [ SmtpAuth.Server.show l ])
            "smtp_auth_server"
            ~meta:()
        ; custom
            (fun l -> l |> List.hd |> SmtpAuth.Port.create)
            (fun l -> [ SmtpAuth.Port.show l ])
            "smtp_auth_port"
            ~meta:()
        ; custom
            (fun l -> l |> List.hd |> SmtpAuth.Username.create)
            (fun l -> [ SmtpAuth.Username.show l ])
            "smtp_auth_username"
            ~meta:()
        ; custom
            (fun l -> l |> List.hd |> SmtpAuth.AuthenticationMethod.create)
            (fun l -> [ SmtpAuth.AuthenticationMethod.show l ])
            "smtp_auth_authentication_method"
            ~meta:()
        ; custom
            (fun l -> l |> List.hd |> SmtpAuth.Protocol.create)
            (fun l -> [ SmtpAuth.Protocol.show l ])
            "smtp_auth_authentication_protocol"
            ~meta:()
        ; custom
            (fun l -> l |> List.hd |> Styles.create)
            (fun l -> [ Styles.show l ])
            "styles"
            ~meta:()
        ; custom
            (fun l -> l |> List.hd |> Icon.create)
            (fun l -> [ Icon.show l ])
            "icon"
            ~meta:()
        ; custom
            (fun l -> l |> List.hd |> Logos.create)
            (fun l -> [ Logos.show l ])
            "logos"
            ~meta:()
        ; custom
            (fun l -> l |> List.hd |> PartnerLogo.create)
            (fun l -> [ PartnerLogo.show l ])
            "partner_logo"
        ; custom
            (fun l -> l |> List.hd |> Settings.Language.of_string)
            (fun l -> [ Settings.Language.show l ])
            "default_language"
            ~meta:()
        ]
        command)
  ;;

  let handle (command : t) =
    let create =
      Ok
        { title = command.title
        ; description = command.description
        ; url = command.url
        ; database = command.database
        ; smtp_auth =
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
    let event (t : Tenant.create) = [ Tenant.Added t |> Pool_event.tenant ] in
    create >|= event
  ;;

  let can user _ =
    Permission.can user ~any_of:[ Permission.Create Permission.Tenant ]
  ;;

  let decode data = Conformist.decode_and_validate schema data
end

module EditTenant : sig
  type t =
    { title : Title.t
    ; description : Description.t
    ; url : Url.t
    ; database : Database.t
    ; smtp_auth_server : SmtpAuth.Server.t
    ; smtp_auth_port : SmtpAuth.Port.t
    ; smtp_auth_username : SmtpAuth.Username.t
    ; smtp_auth_authentication_method : SmtpAuth.AuthenticationMethod.t
    ; smtp_auth_protocol : SmtpAuth.Protocol.t
    ; styles : Styles.t
    ; icon : Icon.t
    ; logos : Logos.t
    ; partner_logos : PartnerLogo.t
    ; disabled : Disabled.t
    ; default_language : Settings.Language.t
    }

  val handle : t -> Tenant.t -> (Pool_event.t list, string) result
  val decode : Conformist.input -> (update, Conformist.error list) result
  val can : Sihl_user.t -> Tenant.t -> bool Lwt.t
end = struct
  type t =
    { title : Title.t
    ; description : Description.t
    ; url : Url.t
    ; database : Database.t
    ; smtp_auth_server : SmtpAuth.Server.t
    ; smtp_auth_port : SmtpAuth.Port.t
    ; smtp_auth_username : SmtpAuth.Username.t
    ; smtp_auth_authentication_method : SmtpAuth.AuthenticationMethod.t
    ; smtp_auth_protocol : SmtpAuth.Protocol.t
    ; styles : Styles.t
    ; icon : Icon.t
    ; logos : Logos.t
    ; partner_logos : PartnerLogo.t
    ; disabled : Disabled.t
    ; default_language : Settings.Language.t
    }

  let command
      title
      description
      url
      database
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
    ; database
    ; smtp_auth =
        { server = smtp_auth_server
        ; port = smtp_auth_port
        ; username = smtp_auth_username
        ; authentication_method = smtp_auth_authentication_method
        ; protocol = smtp_auth_protocol
        }
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
        [ custom
            (fun l -> l |> List.hd |> Title.create)
            (fun l -> [ Title.show l ])
            "title"
        ; custom
            (fun l -> l |> List.hd |> Description.create)
            (fun l -> [ Description.show l ])
            "description"
        ; custom
            (fun l -> l |> List.hd |> Url.create)
            (fun l -> [ Url.show l ])
            "url"
        ; custom
            (fun l -> l |> List.hd |> Database.create)
            (fun l -> [ Database.show l ])
            "daetabase"
        ; custom
            (fun l -> l |> List.hd |> SmtpAuth.Server.create)
            (fun l -> [ SmtpAuth.Server.show l ])
            "smtp_auth_server"
            ~meta:()
        ; custom
            (fun l -> l |> List.hd |> SmtpAuth.Port.create)
            (fun l -> [ SmtpAuth.Port.show l ])
            "smtp_auth_port"
            ~meta:()
        ; custom
            (fun l -> l |> List.hd |> SmtpAuth.Username.create)
            (fun l -> [ SmtpAuth.Username.show l ])
            "smtp_auth_username"
            ~meta:()
        ; custom
            (fun l -> l |> List.hd |> SmtpAuth.AuthenticationMethod.create)
            (fun l -> [ SmtpAuth.AuthenticationMethod.show l ])
            "smtp_auth_authentication_method"
            ~meta:()
        ; custom
            (fun l -> l |> List.hd |> SmtpAuth.Protocol.create)
            (fun l -> [ SmtpAuth.Protocol.show l ])
            "smtp_auth_authentication_protocol"
            ~meta:()
        ; custom
            (fun l -> l |> List.hd |> Styles.create)
            (fun l -> [ Styles.show l ])
            "styles"
            ~meta:()
        ; custom
            (fun l -> l |> List.hd |> Icon.create)
            (fun l -> [ Icon.show l ])
            "icon"
            ~meta:()
        ; custom
            (fun l -> l |> List.hd |> Logos.create)
            (fun l -> [ Logos.show l ])
            "logos"
            ~meta:()
        ; custom
            (fun l -> l |> List.hd |> PartnerLogo.create)
            (fun l -> [ PartnerLogo.show l ])
            "partner_logo"
          (* TODO [timhub]: correctly handle booleands

             https://oxidizing.github.io/conformist/conformist/Conformist/index.html#example
             => Passes boolean as strin "true" *)
        ; custom
            (fun l ->
              l
              |> List.hd
              |> CCString.equal "1"
              |> Disabled.create
              |> CCResult.return)
            (fun l -> [ l |> Disabled.stringify ])
            "disabled"
        ; custom
            (fun l -> l |> List.hd |> Settings.Language.of_string)
            (fun l -> [ Settings.Language.show l ])
            "default_language"
            ~meta:()
        ]
        command)
  ;;

  let handle (command : t) (tenant : Tenant.t) =
    let update =
      Ok
        { title = command.title
        ; description = command.description
        ; url = command.url
        ; database = command.database
        ; smtp_auth =
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
    let event (t : Tenant.update) =
      [ Tenant.Edited (tenant, t) |> Pool_event.tenant ]
    in
    update >|= event
  ;;

  let decode data = Conformist.decode_and_validate schema data

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
