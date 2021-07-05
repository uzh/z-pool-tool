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
    }

  val handle : t -> (Tenant.event list, string) result
  val can : Sihl.User.t -> t -> bool Lwt.t
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
    }

  let handle = Sihl.todo
  let can user _ = Authz.can user ~any_of:[ Authz.Create Authz.Tenant ]
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
    }

  val handle : t -> Tenant.tenant -> (Tenant.event list, string) result
  val can : Sihl.User.t -> t -> bool Lwt.t
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
    }

  let handle = Sihl.todo

  let can user command =
    Authz.can
      user
      ~any_of:[ Authz.Update (Authz.Tenant, Some command.tenant_id) ]
  ;;
end

module Destroy_tenant : sig
  type t = { tenant_id : string }

  val handle : t -> (Tenant.event list, string) result
  val can : Sihl.User.t -> t -> bool Lwt.t
end = struct
  type t = { tenant_id : string }

  let handle = Sihl.todo

  let can user command =
    Authz.can
      user
      ~any_of:[ Authz.Destroy (Authz.Tenant, Some command.tenant_id) ]
  ;;
end

module Add_root : sig
  type t = { user_id : string }

  val handle : t -> Sihl.User.t -> (Tenant.event list, string) result
  val can : Sihl.User.t -> t -> bool Lwt.t
end = struct
  type t = { user_id : string }

  let handle = Sihl.todo
  let can user _ = Authz.can user ~any_of:[ Authz.Manage (Authz.System, None) ]
end

module Add_operator : sig
  type t =
    { user_id : string
    ; tenant_id : string
    }

  val handle
    :  t
    -> Sihl.User.t
    -> Tenant.tenant
    -> (Tenant.event list, string) result

  val can : Sihl.User.t -> t -> bool Lwt.t
end = struct
  type t =
    { user_id : string
    ; tenant_id : string
    }

  let handle = Sihl.todo

  let can user command =
    Authz.can
      user
      ~any_of:
        [ Authz.Manage (Authz.System, None)
        ; Authz.Manage (Authz.Tenant, Some command.tenant_id)
        ]
  ;;
end
