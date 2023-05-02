include Entity
include Event
module Guard = Entity_guard

let find = Repo.find
let find_by_experiment_and_contact_opt = Repo.find_by_experiment_and_contact_opt
let find_by_session = Repo.find_by_session `All
let find_uncanceled_by_session = Repo.find_by_session `Uncanceled
let find_deleted_by_session = Repo.find_by_session `Deleted
let find_with_follow_ups = Repo.find_with_follow_ups
let find_follow_ups = Repo.find_follow_ups

(* TODO: Check that assignment was not marked as deleted? *)
let update_contact_counters_on_cancellation contact assignments =
  let num_no_shows, num_show_ups, num_participations, num_assignments =
    assignments
    |> CCList.fold_left
         (fun (no_shows, show_ups, participations, assignment_count)
              (assignment : t) ->
           let no_shows, show_ups =
             match assignment.no_show |> CCOption.map NoShow.value with
             | Some true -> Contact.NumberOfNoShows.decrement no_shows, show_ups
             | Some false ->
               no_shows, Contact.NumberOfShowUps.decrement show_ups
             | _ -> no_shows, show_ups
           in
           let assignment_count =
             if CCOption.is_some assignment.canceled_at
             then assignment_count
             else Contact.NumberOfAssignments.decrement assignment_count 1
           in
           let participations =
             Contact.NumberOfParticipations.decrement participations
           in
           no_shows, show_ups, participations, assignment_count)
         Contact.(
           ( contact.num_no_shows
           , contact.num_show_ups
           , contact.num_participations
           , contact.num_assignments ))
  in
  Contact.
    { contact with
      num_no_shows
    ; num_show_ups
    ; num_participations
    ; num_assignments
    }
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
