open Entity

type event =
  | Canceled of t
  | Created of (t * Session.Id.t)
  | MarkedAsDeleted of t
  | MatchesFilterUpdated of (t * MatchesFilter.t)
  | ExternalDataIdUpdated of t * ExternalDataId.t option
  | Updated of t * t
[@@deriving eq, show, variants]

let handle_event ?user_uuid pool : event -> unit Lwt.t =
  let create_changelog before after =
    let open Version_history in
    let before = to_record before in
    let after = to_record after in
    insert pool ?user_uuid ~entity_uuid:before.Record.id ~before ~after ()
  in
  function
  | Canceled assignment ->
    let canceleled =
      (* TODO: Check timestamps? Issue #126 *)
      (* TODO: Notification to user? *)
      { assignment with canceled_at = Some (CanceledAt.create_now ()) }
    in
    let%lwt () = create_changelog assignment canceleled in
    Repo.update pool canceleled
  | Created (assignment, session_id) ->
    let open Utils.Lwt_result.Infix in
    let%lwt () = Repo.insert pool session_id assignment in
    Entity_guard.Target.to_authorizable ~ctx:(Database.to_ctx pool) assignment
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : Guard.Target.t) -> ()
  | MarkedAsDeleted assignment ->
    let%lwt () =
      create_changelog
        assignment
        { assignment with marked_as_deleted = MarkedAsDeleted.(create true) }
    in
    (* TODO: Use update query *)
    assignment.id |> Repo.marked_as_deleted pool
  | MatchesFilterUpdated (assignment, matches_filter) ->
    let updated = { assignment with matches_filter } in
    let%lwt () = create_changelog assignment updated in
    Repo.update pool updated
  | ExternalDataIdUpdated (assignment, external_data_id) ->
    let updated = { assignment with external_data_id } in
    let%lwt () = create_changelog assignment updated in
    Repo.update pool updated
  | Updated (before, after) ->
    let%lwt () = create_changelog before after in
    Repo.update pool after
;;
