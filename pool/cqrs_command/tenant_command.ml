module Id = Pool_common.Id

module AssignOperator : sig
  include Common.CommandSig

  type t =
    { user_id : Id.t
    ; tenant_id : Pool_tenant.Id.t
    }

  val handle
    :  Id.t
    -> Admin.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : t -> Guard.ValidationSet.t
end = struct
  type t =
    { user_id : Id.t
    ; tenant_id : Pool_tenant.Id.t
    }

  let handle tenant_id user =
    Ok [ Tenant.OperatorAssigned (tenant_id, user) |> Pool_event.tenant ]
  ;;

  let effects t =
    let open Guard in
    let tenant_id = t.tenant_id |> Uuid.target_of Pool_tenant.Id.value in
    let user_id = t.user_id |> Uuid.target_of Pool_common.Id.value in
    ValidationSet.(
      And
        [ One (Action.Manage, TargetSpec.Id (`Tenant, tenant_id))
        ; One (Action.Manage, TargetSpec.Id (`Admin, user_id))
        ])
  ;;
end

module UnassignOperator : sig
  include Common.CommandSig

  type t =
    { user_id : Id.t
    ; tenant_id : Pool_tenant.Id.t
    }

  val handle
    :  Id.t
    -> Admin.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : t -> Guard.ValidationSet.t
end = struct
  type t =
    { user_id : Id.t
    ; tenant_id : Pool_tenant.Id.t
    }

  let handle tenant_id user =
    Ok [ Tenant.OperatorUnassigned (tenant_id, user) |> Pool_event.tenant ]
  ;;

  let effects t =
    let open Guard in
    let tenant_id = t.tenant_id |> Uuid.target_of Pool_tenant.Id.value in
    let user_id = t.user_id |> Uuid.target_of Pool_common.Id.value in
    ValidationSet.(
      And
        [ One (Action.Manage, TargetSpec.Id (`Tenant, tenant_id))
        ; One (Action.Manage, TargetSpec.Id (`Admin, user_id))
        ])
  ;;
end

module GenerateStatusReport : sig
  include Common.CommandSig

  type t = { tenant_id : Pool_tenant.Id.t }

  val handle
    :  t
    -> Pool_tenant.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : t -> Guard.ValidationSet.t
end = struct
  type t = { tenant_id : Pool_tenant.Id.t }

  let handle = Utils.todo

  let effects t =
    let open Guard in
    let tenant_id = t.tenant_id |> Uuid.target_of Pool_tenant.Id.value in
    ValidationSet.(
      And
        [ One (Action.Manage, TargetSpec.Id (`Tenant, tenant_id))
        ; One (Action.Manage, TargetSpec.Entity `System)
        ])
  ;;
end

module AddRoot : sig
  type t = { user_id : Pool_common.Id.t }

  val handle
    :  t
    -> Sihl_user.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Guard.ValidationSet.t
end = struct
  type t = { user_id : Pool_common.Id.t }

  let handle = Utils.todo

  let effects =
    let open Guard in
    ValidationSet.One (Action.Manage, TargetSpec.Entity `System)
  ;;
end
