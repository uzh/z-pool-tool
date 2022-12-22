module RestoreDefault : sig
  type t = Pool_tenant.t

  val handle
    :  Message_template.t list
    -> unit
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Pool_tenant.t -> Guard.Authorizer.effect list
end = struct
  type t = Pool_tenant.t

  let handle default () =
    Ok
      [ Message_template.(DefaultRestored default)
        |> Pool_event.message_template
      ]
  ;;

  let effects tenant =
    [ ( `Delete
      , `Target
          (tenant |> Pool_tenant.id |> Guard.Uuid.target_of Pool_common.Id.value)
      )
    ]
  ;;
end
