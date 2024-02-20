include Entity
include Event
module Guard = Entity_guard

module Repo = struct
  module Public = struct
    include Repo_public

    module Entity = struct
      include Repo_entity.Public
    end
  end

  include Repo
  module Entity = Repo_entity
end

let find = Repo.find
let find_all = Repo.find_all
let find_all_ids_of_contact_id = Repo.find_all_ids_of_contact_id
let find_public = Repo_public.find
let find_full_by_contact = Repo_public.find_full_by_contact

let find_all_public_by_contact =
  Repo_public.find_all_public_by_contact ~has_session:false
;;

let find_upcoming_to_register =
  Repo_public.find_all_public_by_contact ~has_session:true
;;

let find_pending_waitinglists_by_contact =
  Repo_public.find_pending_waitinglists_by_contact
;;

let find_past_experiments_by_contact =
  Repo_public.find_past_experiments_by_contact
;;

let find_of_session = Repo.find_of_session
let find_of_mailing = Repo.find_of_mailing
let session_count = Repo.session_count
let search = Repo.search
let search_multiple_by_id = Repo.search_multiple_by_id
let find_to_enroll_directly = Repo.find_to_enroll_directly
let contact_is_enrolled = Repo.contact_is_enrolled
let find_targets_grantable_by_admin = Repo.find_targets_grantable_by_admin
let possible_participant_count _ = Lwt.return 0
let possible_participants _ = Lwt.return []

let smtp_auth database_label ({ smtp_auth_id; _ } : t) =
  let open Utils.Lwt_result.Infix in
  match smtp_auth_id with
  | None -> Lwt_result.return None
  | Some id -> Email.SmtpAuth.find database_label id >|+ CCOption.return
;;

let find_contact_person database_label { contact_person_id; _ } =
  let open Utils.Lwt_result.Infix in
  contact_person_id
  |> CCOption.map_or ~default:Lwt.return_none (fun id ->
    id |> Admin.find database_label ||> CCResult.to_opt)
;;

module Statistics = struct
  include Statistics
  module Repo = Repo_statistics

  let create pool id =
    let%lwt registration_possible = Repo.registration_possible pool id in
    let%lwt sending_invitations = Repo.sending_invitations pool id in
    let%lwt session_count = Repo.session_count pool id in
    let%lwt sent_invitation_count = Repo.sent_invitation_count pool id in
    let%lwt showup_count, noshow_count, participation_count =
      Repo.assignment_counts pool id
    in
    Lwt.return
      { registration_possible
      ; sending_invitations
      ; session_count
      ; sent_invitation_count
      ; showup_count
      ; noshow_count
      ; participation_count
      }
  ;;
end
