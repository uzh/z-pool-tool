let src = Logs.Src.create "guard.events"

let log_rules ~tags =
  let open CCFun in
  Lwt_result.map (fun success ->
    Logs.debug ~src (fun m ->
      m
        ~tags
        "Save rules successful: %s"
        Core.Authorizer.([%show: auth_rule list] success));
    success)
  %> Lwt_result.map_error (fun err ->
       Logs.err ~src (fun m ->
         m
           ~tags
           "Save rules failed: %s"
           Core.Authorizer.([%show: auth_rule list] err));
       err)
;;

type event =
  | DefaultRestored of Core.Authorizer.auth_rule list
  | RolesGranted of Repo.Uuid.Actor.t * Core.ActorRoleSet.t
  | RolesRevoked of Repo.Uuid.Actor.t * Core.ActorRoleSet.t
  | RulesSaved of Core.Authorizer.auth_rule list
[@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t =
  let open CCFun in
  let open Utils.Lwt_result.Infix in
  let open Repo in
  let tags = Pool_database.Logger.Tags.create pool in
  let ctx = [ "pool", Pool_database.Label.value pool ] in
  function
  | DefaultRestored permissions ->
    let%lwt (_ : (auth_rule list, auth_rule list) result) =
      Actor.save_rules ~ctx permissions |> log_rules ~tags
    in
    Lwt.return_unit
  | RolesGranted (actor, roles) ->
    let%lwt (_ : (unit, Pool_common.Message.error) result) =
      Actor.grant_roles ~ctx actor roles
      >|- Pool_common.Message.authorization
          %> Pool_common.Utils.with_log_error ~tags
    in
    Lwt.return_unit
  | RolesRevoked (actor, role) ->
    let%lwt (_ : (unit, Pool_common.Message.error) result) =
      Actor.revoke_roles ~ctx actor role
      >|- Pool_common.Message.authorization
          %> Pool_common.Utils.with_log_error ~tags
    in
    Lwt.return_unit
  | RulesSaved rules ->
    let%lwt (_ : (auth_rule list, auth_rule list) result) =
      Actor.save_rules ~ctx rules |> log_rules ~tags
    in
    Lwt.return_unit
;;
