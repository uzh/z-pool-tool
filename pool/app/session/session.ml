include Entity
include Event
module Guard = Entity_guard

let find_all_for_experiment = Repo.find_all_for_experiment
let find_all_public_for_experiment = Repo.find_all_public_for_experiment
let find_all_public_by_location = Repo.find_all_public_by_location
let find = Repo.find
let find_public = Repo.find_public
let find_public_by_assignment = Repo.find_public_by_assignment
let find_by_assignment = Repo.find_by_assignment
let find_experiment_id_and_title = Repo.find_experiment_id_and_title
let find_sessions_to_remind = Repo.find_sessions_to_remind
let find_follow_ups = Repo.find_follow_ups

let has_bookable_spots_for_experiments tenant experiment =
  let open Utils.Lwt_result.Infix in
  find_all_for_experiment tenant experiment
  >|+ CCList.filter (fun session ->
        CCOption.is_none session.Entity.follow_up_to
        && not (Entity.is_fully_booked session))
  >|+ CCList.is_empty
  >|+ not
;;
