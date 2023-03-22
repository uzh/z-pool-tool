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
    ValidationSet.(
      And
        [ One (Action.Manage, TargetSpec.Id (`Tenant, tenant_id))
        ; SpecificRole `ManageOperators
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
    ValidationSet.(
      And
        [ One (Action.Manage, TargetSpec.Id (`Tenant, tenant_id))
        ; SpecificRole `ManageOperators
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
