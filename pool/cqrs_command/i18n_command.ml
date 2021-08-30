module Update : sig
  type t = { new_settings : (string * string) list }

  val handle : t -> (I18n.event list, string) Result.t
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = { new_settings : (string * string) list }

  let handle = Utils.todo

  let can user _ =
    let open Lwt.Syntax in
    let* tenant = Tenant.find_by_user user in
    Permission.can
      user
      ~any_of:
        [ Permission.Update
            (Permission.Tenant, Some (tenant |> Tenant.Id.to_human))
        ]
  ;;
end
