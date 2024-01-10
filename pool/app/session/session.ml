include Entity
include Event
module Guard = Entity_guard

let find_all_for_experiment = Repo.find_all_for_experiment

let find_all_to_assign_from_waitinglist =
  Repo.find_all_to_assign_from_waitinglist
;;

let find_all_public_for_experiment = Repo.find_all_public_for_experiment
let find_all_public_by_location = Repo.find_all_public_by_location
let find_all_ids_of_contact_id = Repo.find_all_ids_of_contact_id
let find = Repo.find
let find_multiple = Repo.find_multiple

let find_contact_is_assigned_by_experiment =
  Repo.find_contact_is_assigned_by_experiment
;;

let find_public = Repo.find_public
let find_public_by_assignment = Repo.find_public_by_assignment
let find_upcoming_public_by_contact = Repo.find_upcoming_public_by_contact
let find_by_assignment = Repo.find_by_assignment
let find_experiment_id_and_title = Repo.find_experiment_id_and_title
let find_sessions_to_remind = Repo.find_sessions_to_remind
let find_follow_ups = Repo.find_follow_ups
let find_open_with_follow_ups = Repo.find_open_with_follow_ups
let find_open = Repo.find_open
let find_for_calendar_by_location = Repo.find_for_calendar_by_location
let find_for_calendar_by_user = Repo.find_for_calendar_by_user

let has_bookable_spots_for_experiments tenant experiment =
  let open Utils.Lwt_result.Infix in
  find_all_for_experiment tenant experiment
  ||> CCList.filter (fun session ->
    CCOption.is_none session.Entity.follow_up_to
    && not (Entity.is_fully_booked session))
  ||> CCList.is_empty
  ||> not
;;

let find_all_to_swap_by_experiment database_label experiment_id =
  let open Utils.Lwt_result.Infix in
  find_all_for_experiment database_label experiment_id
  ||> group_and_sort
  ||> CCList.fold_left
        (fun sessions (parent, followups) ->
          parent :: followups
          |> fun list ->
          CCList.find_opt
            CCFun.(can_be_assigned_to_existing_assignment %> CCResult.is_ok)
            list
          |> function
          | None -> sessions
          | Some _ -> sessions @ list)
        []
;;

module Repo = struct
  module Id = Repo_entity.Id
  module Start = Repo_entity.Start
  module Duration = Repo_entity.Duration
end
