open CCResult.Infix

module Add_tenant : sig
  type t =
    { title : string
    ; description : string
    ; url : string
    ; database : string
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
    ; styles : string
    ; icon : string
    ; logos : string
    ; partner_logos : string
    ; disabled : bool
    ; default_language : string
    }

  let handle (t : t) =
    let open Tenant in
    let go () =
      let open CCResult in
      let* title = Title.create t.title in
      let* description = Description.create t.description in
      let* url = Url.create t.url in
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
        ; styles
        ; icon
        ; logos
        ; partner_logos
        ; disabled
        ; default_language
        }
    in
    (* TODO [timhub]: Type error *)
    () |> go |> failwith ""
  ;;

  (* let event (t : Tenant.create) = Tenant.Added t in () |> go >|= event *)

  let can user _ =
    Permission.can user ~any_of:[ Permission.Create Permission.Tenant ]
  ;;
end

module Edit_tenant : sig
  type t =
    { tenant_id : string
    ; title : string
    ; description : string
    ; url : string
    ; database : string
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
    ; styles : string
    ; icon : string
    ; logos : string
    ; partner_logos : string
    ; disabled : bool
    ; default_language : string
    }

  let handle (t : t) (tenant : Tenant.t) =
    let open Tenant in
    let go () =
      let open CCResult in
      let* title = Title.create t.title in
      let* description = Description.create t.description in
      let* url = Url.create t.url in
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
        ; styles
        ; icon
        ; logos
        ; partner_logos
        ; disabled
        ; default_language
        }
    in
    let event (t : Tenant.update) = Tenant.Edited (tenant, t) in
    () |> go >|= event
  ;;

  let can user command =
    Permission.can
      user
      ~any_of:[ Permission.Update (Permission.Tenant, Some command.tenant_id) ]
  ;;
end

module Destroy_tenant : sig
  type t = { tenant_id : string }

  val handle : t -> (Tenant.event, string) Lwt_result.t
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = { tenant_id : string }

  let handle t =
    let open Lwt_result.Infix in
    let go () = t.tenant_id |> Tenant.find_by_id in
    let event (t : Tenant.t) = Tenant.Destroyed t in
    () |> go >|= event
  ;;

  let can user command =
    Permission.can
      user
      ~any_of:[ Permission.Destroy (Permission.Tenant, Some command.tenant_id) ]
  ;;
end

module Disable_tenant : sig
  type t = { tenant_id : string }

  val handle : t -> (Tenant.event, string) Lwt_result.t
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = { tenant_id : string }

  let handle t =
    let open Lwt_result.Infix in
    let go () = t.tenant_id |> Tenant.find_by_id in
    let event (t : Tenant.t) = Tenant.Disabled t in
    () |> go >|= event
  ;;

  let can user command =
    Permission.can
      user
      ~any_of:[ Permission.Destroy (Permission.Tenant, Some command.tenant_id) ]
  ;;
end

module Enable_tenant : sig
  type t = { tenant_id : string }

  val handle : t -> (Tenant.event, string) Lwt_result.t
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = { tenant_id : string }

  let handle t =
    let open Lwt_result.Infix in
    let go () = t.tenant_id |> Tenant.find_by_id in
    let event (t : Tenant.t) = Tenant.Enabled t in
    () |> go >|= event
  ;;

  let can user command =
    Permission.can
      user
      ~any_of:[ Permission.Destroy (Permission.Tenant, Some command.tenant_id) ]
  ;;
end

module Assign_operator : sig
  type t =
    { user_id : string
    ; tenant_id : string
    }

  val handle : t -> Sihl_user.t -> Tenant.t -> (Tenant.event, string) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { user_id : string
    ; tenant_id : string
    }

  let handle _ user tenant = Ok (Tenant.OperatorAssigned (tenant, user))

  let can user command =
    Permission.can
      user
      ~any_of:
        [ Permission.Manage (Permission.System, None)
        ; Permission.Manage (Permission.Tenant, Some command.tenant_id)
        ]
  ;;
end

module Divest_operator : sig
  (* TODO [timhub]: pass ids or User & Tenant => see module Add_operator *)

  (* val handle : t -> Sihl_user.t -> Tenant.t -> (Tenant.event, string)
     result *)
  (* let handle _ user tenant = Ok (Tenant.OperatorDivested (tenant, user)) *)
  type t =
    { user_id : string
    ; tenant_id : string
    }

  val handle : t -> (Tenant.event, string) result Lwt.t
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { user_id : string
    ; tenant_id : string
    }

  let handle t =
    let open CCResult.Infix in
    let open Lwt.Syntax in
    let* user = Service.User.find t.user_id in
    let* tenant = Tenant.find_by_id t.tenant_id in
    let event (user : Sihl_user.t) (tenant : Tenant.t) =
      Ok (Tenant.OperatorDivested (tenant, user))
    in
    tenant >>= event user |> Lwt_result.lift
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

module Generate_status_report : sig
  type t = { tenant_id : string }

  val handle : t -> Tenant.t -> (Tenant.event list, string) result
end = struct
  type t = { tenant_id : string }

  let handle = Utils.todo
end

module Add_root : sig
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
