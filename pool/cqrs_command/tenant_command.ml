module GenerateStatusReport : sig
  include Common.CommandSig

  type t = { tenant_id : Pool_tenant.Id.t }

  val handle : t -> Pool_tenant.t -> (Pool_event.t list, Pool_message.Error.t) result
  val effects : Pool_tenant.Id.t -> Guard.ValidationSet.t
end = struct
  type t = { tenant_id : Pool_tenant.Id.t }

  let handle = Utils.todo

  let effects id =
    let open Guard in
    ValidationSet.(
      And
        [ Pool_tenant.Guard.Access.update id
        ; one_of_tuple (Permission.Manage, `System, None)
        ])
  ;;
end
