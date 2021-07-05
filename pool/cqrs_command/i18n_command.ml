module Update : sig
  type t = { new_settings : (string * string) list }

  val handle : t -> (I18n.event list, string) Result.t
  val can : Sihl.User.t -> t -> bool Lwt.t
end = struct
  type t = { new_settings : (string * string) list }

  let handle = Sihl.todo

  let can user _ =
    let open Lwt.Syntax in
    let* tenant = Tenant.find_by_user user in
    Authz.can
      user
      ~any_of:[ Authz.Update (Authz.Tenant, Some tenant.Tenant.id) ]
  ;;
end
