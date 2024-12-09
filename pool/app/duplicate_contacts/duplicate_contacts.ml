include Entity
include Event
include Entity_guard

let run database_label contact_uuid =
  let open Utils.Lwt_result.Infix in
  let%lwt fields = Custom_field.find_for_duplicate_check database_label in
  Repo.find_similars database_label ~user_uuid:contact_uuid fields
  >|> Repo.insert database_label
;;

let find = Repo.find
let all = Repo.all
let find_by_contact = Repo.find_by_contact

(** Data that has to be moved from the merge_contact to the new one:

    Counts:
    - invitation_count
    - show_up_count
    - no_show_count
    - participation_count

    Rows:
    - invitations
    - waiting list
    - assignments
    - tags
    - x pool_queue_jobs_mapping
    - x changelog

    Open discussion:
    - duplicates (Or just delete the old ones) *)

let merge pool ({ contact; merged_contact; _ } as merge) =
  let%lwt invitations =
    Invitation.find_by_contact_to_merge pool ~contact ~merged_contact
  in
  let%lwt waiting_list =
    Waiting_list.find_by_contact_to_merge pool ~contact ~merged_contact
  in
  let%lwt assignments =
    Assignment.find_by_contact_to_merge pool ~contact ~merged_contact
  in
  let contact =
    let open Contact in
    contact
    |> update_num_invitations ~step:(CCList.length invitations)
    |> update_num_assignments ~step:(CCList.length assignments)
    |> update_num_no_shows ~step:(merged_contact.num_no_shows |> NumberOfNoShows.value)
    |> update_num_participations
         ~step:(merged_contact.num_participations |> NumberOfParticipations.value)
  in
  Repo_merge.merge pool { merge with contact } invitations waiting_list assignments
;;
