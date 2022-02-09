module RestoreDefault : sig
  type t = Pool_tenant.t

  val handle
    :  Email.default
    -> unit
    -> (Pool_event.t list, Pool_common.Message.error) result
end = struct
  type t = Pool_tenant.t

  let handle default () =
    Ok [ Email.(DefaultRestored default) |> Pool_event.email_address ]
  ;;
end
