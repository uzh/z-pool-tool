include Entity
include Event
module Guard = Entity_guard

let find = Repo.find

let find_upcoming_by_experiment_and_contact_opt =
  Repo.find_by_experiment_and_contact_opt `Upcoming
;;

let find_past_by_experiment_and_contact_opt =
  Repo.find_by_experiment_and_contact_opt `Past
;;

let find_all_by_experiment_and_contact_opt =
  Repo.find_by_experiment_and_contact_opt `All
;;

let find_by_session = Repo.find_by_session `All
let find_uncanceled_by_session = Repo.find_by_session `Uncanceled
let find_deleted_by_session = Repo.find_by_session `Deleted
let find_with_follow_ups = Repo.find_with_follow_ups
let find_follow_ups = Repo.find_follow_ups

let contact_participation_in_other_assignments =
  Repo.contact_participation_in_other_assignments
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
