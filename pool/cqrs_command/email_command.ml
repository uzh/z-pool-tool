module RestoreDefault : sig
  type t = Pool_tenant.t

  val handle
    :  Email.default
    -> unit
    -> (Pool_event.t list, Pool_common.Message.error) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = Pool_tenant.t

  let handle default () =
    Ok [ Email.(DefaultRestored default) |> Pool_event.email ]
  ;;

  let can user command =
    Permission.can
      user
      ~any_of:
        [ Permission.Destroy
            (Permission.Tenant, Some (command |> Pool_tenant.id))
        ]
  ;;
end
