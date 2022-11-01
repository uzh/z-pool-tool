module RestoreDefault : sig
  type t = Pool_tenant.t

  val handle
    :  Email.default
    -> unit
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Pool_tenant.t -> Guard.Authorizer.effect list
end = struct
  type t = Pool_tenant.t

  let handle default () =
    Ok [ Email.(DefaultRestored default) |> Pool_event.email ]
  ;;

  let effects tenant =
    [ `Delete, `One (tenant |> Pool_tenant.id |> Pool_common.Id.to_uuidm) ]
  ;;
end
