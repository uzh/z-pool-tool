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

  val handle : t -> (Pool_event.t list, string) result
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
    }

  let handle = Utils.todo

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
    }

  val handle : t -> Tenant.t -> (Pool_event.t list, string) result
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
    }

  let handle = Utils.todo

  let can user command =
    Permission.can
      user
      ~any_of:[ Permission.Update (Permission.Tenant, Some command.tenant_id) ]
  ;;
end

module Destroy_tenant : sig
  type t = { tenant_id : string }

  val handle : t -> (Pool_event.t list, string) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = { tenant_id : string }

  let handle = Utils.todo

  let can user command =
    Permission.can
      user
      ~any_of:[ Permission.Destroy (Permission.Tenant, Some command.tenant_id) ]
  ;;
end

module Add_root : sig
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

module Add_operator : sig
  type t =
    { user_id : string
    ; tenant_id : string
    }

  val handle
    :  t
    -> Sihl_user.t
    -> Tenant.t
    -> (Pool_event.t list, string) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { user_id : string
    ; tenant_id : string
    }

  let handle = Utils.todo

  let can user command =
    Permission.can
      user
      ~any_of:
        [ Permission.Manage (Permission.System, None)
        ; Permission.Manage (Permission.Tenant, Some command.tenant_id)
        ]
  ;;
end
