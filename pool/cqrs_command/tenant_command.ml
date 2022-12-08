module Id = Pool_common.Id

module AssignOperator : sig
  type t =
    { user_id : Id.t
    ; tenant_id : Id.t
    }

  val handle
    :  Id.t
    -> Admin.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val can : t -> Guard.Authorizer.effect list
end = struct
  type t =
    { user_id : Id.t
    ; tenant_id : Id.t
    }

  let handle tenant_id user =
    Ok [ Tenant.OperatorAssigned (tenant_id, user) |> Pool_event.tenant ]
  ;;

  let can t =
    [ `Manage, `Target (t.user_id |> Guard.Uuid.target_of Id.value)
    ; `Manage, `Target (t.tenant_id |> Guard.Uuid.target_of Id.value)
    ]
  ;;
end

module DivestOperator : sig
  (* TODO: Type safety *)
  type t =
    { user_id : string
    ; tenant_id : string
    }

  val handle
    :  Id.t
    -> Admin.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val can : t -> Guard.Authorizer.effect list
end = struct
  type t =
    { user_id : string
    ; tenant_id : string
    }

  let handle tenant_id user =
    Ok [ Tenant.OperatorDivested (tenant_id, user) |> Pool_event.tenant ]
  ;;

  let can t =
    [ `Manage, `Target (t.user_id |> Guard.Uuid.Target.of_string_exn)
    ; `Manage, `Target (t.tenant_id |> Guard.Uuid.Target.of_string_exn)
    ]
  ;;
end

module GenerateStatusReport : sig
  (* TODO: Type safety *)
  type t = { tenant_id : string }

  val handle
    :  t
    -> Pool_tenant.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val can : t -> Guard.Authorizer.effect list
end = struct
  type t = { tenant_id : string }

  let handle = Utils.todo

  let can t =
    [ `Manage, `TargetEntity `System
    ; `Manage, `Target (t.tenant_id |> Guard.Uuid.Target.of_string_exn)
    ]
  ;;
end

module AddRoot : sig
  (* TODO: Type safety *)
  type t = { user_id : string }

  val handle
    :  t
    -> Sihl_user.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val can : Guard.Authorizer.effect list
end = struct
  type t = { user_id : string }

  let handle = Utils.todo
  let can = [ `Manage, `TargetEntity `System ]
end
