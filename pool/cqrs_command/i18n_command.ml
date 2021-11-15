module Update : sig
  type t = { new_settings : (string * string) list }

  val handle : t -> (Pool_event.t list, Pool_common.Message.error) result
  val can : Pool_common.Database.Label.t -> 'admin Admin.t -> t -> bool Lwt.t
end = struct
  type t = { new_settings : (string * string) list }

  let handle = Utils.todo

  let can
      : type admin.
        Pool_common.Database.Label.t -> admin Admin.t -> t -> bool Lwt.t
    =
   fun pool admin _ ->
    let open Utils.Lwt_result.Infix in
    let check_permission tenant =
      Permission.can
        (Admin.user admin)
        ~any_of:[ Permission.Update (Permission.Tenant, Some tenant.Tenant.id) ]
    in
    pool
    |> Tenant.find_by_label
    |>> check_permission
    |> Lwt.map (CCResult.get_or ~default:false)
 ;;
end
