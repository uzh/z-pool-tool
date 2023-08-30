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
        ; One (Permission.Manage, TargetEntity.Model `System)
        ])
  ;;
end
