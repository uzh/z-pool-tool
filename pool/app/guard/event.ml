type event = DefaultRestored of Core.Authorizer.auth_rule list
[@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t =
  let open Repo in
  let ctx = Pool_tenant.to_ctx pool in
  function
  | DefaultRestored permissions ->
    let%lwt (_ : (auth_rule list, auth_rule list) result) =
      Actor.save_rules ~ctx permissions
    in
    Lwt.return_unit
;;
