module RestoreDefault : sig
  type t = Pool_tenant.t

  val handle
    :  Pool_database.Label.t
    -> unit
    -> (Pool_event.t list, Pool_common.Message.error) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = Pool_tenant.t

  let handle pool () =
    Ok [ Email.(DefaultRestored pool) |> Pool_event.email_address ]
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
