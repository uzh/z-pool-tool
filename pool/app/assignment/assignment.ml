include Entity
include Event
module Guard = Entity_guard
module Service = Service

let find = Repo.find
let find_closed = Repo.find_closed

let find_upcoming_public_by_experiment_and_contact_opt =
  Repo.find_public_by_experiment_and_contact_opt `Upcoming
;;

let find_past_public_by_experiment_and_contact_opt =
  Repo.find_public_by_experiment_and_contact_opt `Past
;;

let find_all_public_by_experiment_and_contact_opt =
  Repo.find_public_by_experiment_and_contact_opt `All
;;

let find_by_contact_and_experiment = Repo.Sql.find_by_contact_and_experiment
let find_not_deleted_by_session = Repo.find_not_deleted_by_session
let find_all_by_session = Repo.find_all_by_session
let find_multiple_by_session = Repo.Sql.find_multiple_by_session
let query_by_session = Repo.query_by_session
let find_uncanceled_by_session = Repo.find_uncanceled_by_session

let find_for_session_close_screen pool session_id =
  let open Utils.Lwt_result.Infix in
  Repo.find_uncanceled_by_session pool session_id
  >|> Repo.enrich_with_customfield_data `SessionClose pool
;;

let find_for_session_detail_screen ~query pool session_id =
  let%lwt assignments, query = Repo.query_by_session ~query pool session_id in
  let%lwt assignments =
    assignments |> Repo.enrich_with_customfield_data `SessionDetail pool
  in
  Lwt.return (assignments, query)
;;

let find_deleted_by_session = Repo.find_deleted_by_session
let find_with_follow_ups = Repo.find_with_follow_ups
let find_follow_ups = Repo.find_follow_ups

let contact_participation_in_other_assignments =
  Repo.contact_participation_in_other_assignments
;;

let find_external_data_identifiers_by_contact =
  Repo_external_data_identifier.find_by_contact
;;

let group_by_contact list =
  let tbl = Hashtbl.create 20 in
  List.iter
    (fun ({ contact; _ } as m : t) ->
      let open CCOption in
      Hashtbl.find_opt tbl contact
      >|= CCList.cons m
      |> value ~default:[ m ]
      |> Hashtbl.replace tbl contact)
    list;
  Hashtbl.fold (fun contact lst acc -> (contact, lst) :: acc) tbl []
;;

type session_counters =
  { total : int
  ; num_no_shows : int
  ; num_participations : int
  }

let init_session_counters =
  { total = 0; num_no_shows = 0; num_participations = 0 }
;;

let assignments_to_session_counters =
  CCList.fold_left
    (fun { total; num_no_shows; num_participations }
      ({ no_show; participated; _ } : t) ->
      let default = CCOption.value ~default:false in
      { total = total + 1
      ; num_no_shows =
          (if default no_show then num_no_shows + 1 else num_no_shows)
      ; num_participations =
          (if default participated
           then num_participations + 1
           else num_participations)
      })
    init_session_counters
;;

let counters_of_session database_label session_id =
  let open Utils.Lwt_result.Infix in
  find_uncanceled_by_session database_label session_id
  ||> assignments_to_session_counters
;;

let update_matches_filter = Service.update_matches_filter
let update_matches_filter_events = Service.update_matches_filter_events
