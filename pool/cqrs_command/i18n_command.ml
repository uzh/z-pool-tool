module Update : sig
  type t = { new_settings : (string * string) list }

  val handle : t -> (Pool_event.t list, Pool_common.Message.error) result
  val can : Database_pool.Label.t -> 'admin Admin.t -> t -> bool Lwt.t
end = struct
  type t = { new_settings : (string * string) list }

  let handle = Utils.todo

  let can
      : type admin. Database_pool.Label.t -> admin Admin.t -> t -> bool Lwt.t
    =
   fun pool admin _ ->
    let open Utils.Lwt_result.Infix in
    let check_permission tenant =
      Permission.can
        (Admin.user admin)
        ~any_of:
          [ Permission.Update (Permission.Tenant, Some tenant.Tenant_pool.id) ]
    in
    pool
    |> Tenant_pool.find_by_label
    |>> check_permission
    |> Lwt.map (CCResult.get_or ~default:false)
 ;;
end
