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

  val effects : Pool_tenant.Id.t -> Guard.ValidationSet.t
end = struct
  type t =
    { user_id : Id.t
    ; tenant_id : Pool_tenant.Id.t
    }

  let handle tenant_id user =
    Ok [ Tenant.OperatorAssigned (tenant_id, user) |> Pool_event.tenant ]
  ;;

  let effects id =
    let open Guard in
    ValidationSet.(
      And [ Pool_tenant.Guard.Access.update id; SpecificRole `ManageOperators ])
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

  val effects : Pool_tenant.Id.t -> Guard.ValidationSet.t
end = struct
  type t =
    { user_id : Id.t
    ; tenant_id : Pool_tenant.Id.t
    }

  let handle tenant_id user =
    Ok [ Tenant.OperatorUnassigned (tenant_id, user) |> Pool_event.tenant ]
  ;;

  let effects id =
    let open Guard in
    ValidationSet.(
      And [ Pool_tenant.Guard.Access.update id; SpecificRole `ManageOperators ])
  ;;
end

module GenerateStatusReport : sig
  include Common.CommandSig

  type t = { tenant_id : Pool_tenant.Id.t }

  val handle
    :  t
    -> Pool_tenant.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Pool_tenant.Id.t -> Guard.ValidationSet.t
end = struct
  type t = { tenant_id : Pool_tenant.Id.t }

  let handle = Utils.todo

  let effects id =
    let open Guard in
    ValidationSet.(
      And
        [ Pool_tenant.Guard.Access.update id
        ; One (Action.Manage, TargetSpec.Entity `System)
        ])
  ;;
end
