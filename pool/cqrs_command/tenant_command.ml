module Id = Pool_common.Id

module AssignOperator : sig
  type t =
    { user_id : Id.t
    ; tenant_id : Id.t
    }

  val handle
    :  Id.t
    -> Admin.operator Admin.t
    -> (Pool_event.t list, Pool_common.Message.error) result
end = struct
  type t =
    { user_id : Id.t
    ; tenant_id : Id.t
    }

  let handle tenant_id user =
    Ok [ Tenant.OperatorAssigned (tenant_id, user) |> Pool_event.tenant ]
  ;;

  let _effects = [ `Manage, `Role `System; `Manage, `Uniq "command.tenant_id" ]
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
end = struct
  type t =
    { user_id : string
    ; tenant_id : string
    }

  let handle tenant_id user =
    Ok [ Tenant.OperatorDivested (tenant_id, user) |> Pool_event.tenant ]
  ;;

  let _effects = [ `Manage, `Role `System; `Manage, `Uniq "command.tenant_id" ]
end

module GenerateStatusReport : sig
  type t = { tenant_id : string }

  val handle
    :  t
    -> Pool_tenant.t
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
end = struct
  type t = { user_id : string }

  let handle = Utils.todo
  let _effects = [ `Manage, `Role `System ]
end
