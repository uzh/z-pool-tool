open Core

let src = Logs.Src.create "guard.events"

let log_rules ~tags =
  let open CCFun in
  Lwt_result.map (fun success ->
    Logs.debug ~src (fun m ->
      m ~tags "Save rules successful: %s" ([%show: Rule.t list] success));
    success)
  %> Lwt_result.map_error (fun err ->
       Logs.err ~src (fun m ->
         m ~tags "Save rules failed: %s" ([%show: Rule.t list] err));
       err)
;;

type event =
  | DefaultRestored of Rule.t list
  | RolesGranted of Repo.Uuid.Actor.t * RoleSet.t
  | RolesRevoked of Repo.Uuid.Actor.t * RoleSet.t
  | RulesSaved of Rule.t list
  | RuleDeleted of Rule.t
[@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t =
  let open CCFun in
  let open Utils.Lwt_result.Infix in
  let tags = Pool_database.Logger.Tags.create pool in
  let ctx = [ "pool", Pool_database.Label.value pool ] in
  function
  | DefaultRestored permissions ->
    let%lwt (_ : (Rule.t list, Rule.t list) result) =
      Repo.Rule.save_all ~ctx permissions |> log_rules ~tags
    in
    Lwt.return_unit
  | RolesGranted (actor, roles) ->
    let%lwt (_ : (unit, Pool_common.Message.error) result) =
      Repo.Actor.grant_roles ~ctx actor roles
      >|- Pool_common.(Message.authorization %> Utils.with_log_error ~src ~tags)
    in
    Lwt.return_unit
  | RolesRevoked (actor, role) ->
    let%lwt (_ : (unit, Pool_common.Message.error) result) =
      Repo.Actor.revoke_roles ~ctx actor role
      >|- Pool_common.(Message.authorization %> Utils.with_log_error ~src ~tags)
    in
    Lwt.return_unit
  | RulesSaved rules ->
    let%lwt (_ : (Rule.t list, Rule.t list) result) =
      Repo.Rule.save_all ~ctx rules |> log_rules ~tags
    in
    Lwt.return_unit
  | RuleDeleted rule ->
    let%lwt (_ : (unit, string) result) =
      Repo.Rule.delete ~ctx rule
      ||> Pool_common.(
            Utils.with_log_result_error ~src ~tags Message.authorization)
    in
    Lwt.return_unit
;;
