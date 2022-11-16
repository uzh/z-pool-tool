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
    [ ( `Delete
      , `Target
          (tenant |> Pool_tenant.id |> Guard.Uuid.target_of Pool_common.Id.value)
      )
    ]
  ;;
end
