open CCResult.Infix

module AddTenant : sig
  type t =
    { title : string
    ; description : string
    ; url : string
    ; database : string
    ; smtp_server : string
    ; smtp_port : string
    ; smtp_username : string
    ; smtp_authentication_method : string
    ; smtp_protocol : string
    ; styles : string
    ; icon : string
    ; logos : string
    ; partner_logos : string
    ; disabled : bool
    ; default_language : string
    }

  val handle : t -> (Tenant.event, string) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { title : string
    ; description : string
    ; url : string
    ; database : string
    ; smtp_server : string
    ; smtp_port : string
    ; smtp_username : string
    ; smtp_authentication_method : string
    ; smtp_protocol : string
    ; styles : string
    ; icon : string
    ; logos : string
    ; partner_logos : string
    ; disabled : bool
    ; default_language : string
    }

  let handle (t : t) =
    let open Tenant in
    let create =
      let open CCResult in
      let* title = Title.create t.title in
      let* description = Description.create t.description in
      let* url = Url.create t.url in
      let* smtp_auth =
        SmtpAuth.create
          t.smtp_server
          t.smtp_port
          t.smtp_username
          t.smtp_authentication_method
          t.smtp_protocol
          ()
      in
      let* database = Database.create t.database in
      let* styles = Styles.create t.styles in
      let* icon = Icon.create t.icon in
      let* logos = Logos.create t.logos in
      let* partner_logos = PartnerLogo.create t.partner_logos in
      (* let* disabled = Disabled.create t.disabled in *)
      let* default_language = Settings.Language.of_string t.default_language in
      Ok
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
    let event (t : Tenant.create) = Tenant.Added t in
    create >|= event
  ;;

  let can user _ =
    Permission.can user ~any_of:[ Permission.Create Permission.Tenant ]
  ;;
end

module EditTenant : sig
  type t =
    { tenant_id : string
    ; title : string
    ; description : string
    ; url : string
    ; database : string
    ; smtp_server : string
    ; smtp_port : string
    ; smtp_username : string
    ; smtp_authentication_method : string
    ; smtp_protocol : string
    ; styles : string
    ; icon : string
    ; logos : string
    ; partner_logos : string
    ; disabled : bool
    ; default_language : string
    }

  val handle : t -> Tenant.t -> (Tenant.event, string) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { tenant_id : string
    ; title : string
    ; description : string
    ; url : string
    ; database : string
    ; smtp_server : string
    ; smtp_port : string
    ; smtp_username : string
    ; smtp_authentication_method : string
    ; smtp_protocol : string
    ; styles : string
    ; icon : string
    ; logos : string
    ; partner_logos : string
    ; disabled : bool
    ; default_language : string
    }

  let handle (t : t) (tenant : Tenant.t) =
    let open Tenant in
    let update =
      let open CCResult in
      let* title = Title.create t.title in
      let* description = Description.create t.description in
      let* url = Url.create t.url in
      let* smtp_auth =
        SmtpAuth.create
          t.smtp_server
          t.smtp_port
          t.smtp_username
          t.smtp_authentication_method
          t.smtp_protocol
          ()
      in
      let* database = Database.create t.database in
      let* styles = Styles.create t.styles in
      let* icon = Icon.create t.icon in
      let* logos = Logos.create t.logos in
      let* partner_logos = PartnerLogo.create t.partner_logos in
      let* disabled = Disabled.create t.disabled in
      let* default_language = Settings.Language.of_string t.default_language in
      Ok
        { title
        ; description
        ; url
        ; database
        ; smtp_auth
        ; styles
        ; icon
        ; logos
        ; partner_logos
        ; disabled
        ; default_language
        }
    in
    let event (t : Tenant.update) = Tenant.Edited (tenant, t) in
    update >|= event
  ;;

  let can user command =
    Permission.can
      user
      ~any_of:[ Permission.Update (Permission.Tenant, Some command.tenant_id) ]
  ;;
end

module DestroyTenant : sig
  type t = { tenant_id : string }

  val handle : t -> (Tenant.event, 'a) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = { tenant_id : string }

  let handle t = Ok (Tenant.Destroyed t.tenant_id)

  let can user command =
    Permission.can
      user
      ~any_of:[ Permission.Destroy (Permission.Tenant, Some command.tenant_id) ]
  ;;
end

module DisableTenant : sig
  type t = { tenant_id : string }

  val handle : Tenant.t -> (Tenant.event, 'b) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = { tenant_id : string }

  let handle tenant = Ok (Tenant.Disabled tenant)

  let can user command =
    Permission.can
      user
      ~any_of:[ Permission.Destroy (Permission.Tenant, Some command.tenant_id) ]
  ;;
end

module EnableTenant : sig
  type t = { tenant_id : string }

  val handle : Tenant.t -> (Tenant.event, 'b) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = { tenant_id : string }

  let handle tenant = Ok (Tenant.Enabled tenant)

  let can user command =
    Permission.can
      user
      ~any_of:[ Permission.Destroy (Permission.Tenant, Some command.tenant_id) ]
  ;;
end

module AssignOperator : sig
  type t =
    { user_id : string
    ; tenant_id : string
    }

  val handle : Tenant.t -> Sihl_user.t -> (Tenant.event, string) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { user_id : string
    ; tenant_id : string
    }

  let handle tenant user = Ok (Tenant.OperatorAssigned (tenant, user))

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

  val handle : Tenant.t -> Sihl_user.t -> (Tenant.event, string) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { user_id : string
    ; tenant_id : string
    }

  let handle tenant user = Ok (Tenant.OperatorDivested (tenant, user))

  let can user command =
    Permission.can
      user
      ~any_of:
        [ Permission.Manage (Permission.System, None)
        ; Permission.Manage (Permission.Tenant, Some command.tenant_id)
        ]
  ;;
end

module GenerateStatusReport : sig
  type t = { tenant_id : string }

  val handle : t -> Tenant.t -> (Tenant.event list, string) result
end = struct
  type t = { tenant_id : string }

  let handle = Utils.todo
end

module AddRoot : sig
  type t = { user_id : string }

  val handle : t -> Sihl_user.t -> (Tenant.event list, string) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = { user_id : string }

  let handle = Utils.todo

  let can user _ =
    Permission.can user ~any_of:[ Permission.Manage (Permission.System, None) ]
  ;;
end
