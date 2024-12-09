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

(** Open discussion:
    - duplicates (Or just delete the old ones) *)

let merge pool ?user_uuid ({ contact; merged_contact; _ } as merge) =
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
  Repo_merge.merge
    pool
    ?user_uuid
    { merge with contact }
    invitations
    waiting_list
    assignments
;;
