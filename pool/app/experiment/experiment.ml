include Entity
include Event
module Guardian = Guard
module Guard = Entity_guard
module VersionHistory = Version_history

module Repo = struct
  module Public = struct
    include Repo_public

    module Entity = struct
      include Repo_entity.Public
    end
  end

  module Entity = Repo_entity
  include Repo
end

let find = Repo.find
let all = Repo.all
let list_by_user = Repo.Sql.list_by_user
let find_all_ids_of_contact_id = Repo.find_all_ids_of_contact_id
let find_public = Repo_public.find
let find_full_by_contact = Repo_public.find_full_by_contact
let find_upcoming_to_register = Repo_public.find_upcoming_to_register

let find_pending_waitinglists_by_contact =
  Repo_public.find_pending_waitinglists_by_contact
;;

let find_past_experiments_by_contact = Repo_public.find_past_experiments_by_contact
let find_of_session = Repo.find_of_session
let find_of_mailing = Repo.find_of_mailing
let session_count = Repo.session_count
let search = Repo.search
let search_multiple_by_id = Repo.search_multiple_by_id
let find_to_enroll_directly = Repo.find_to_enroll_directly
let contact_is_enrolled = Repo.contact_is_enrolled
let find_targets_grantable_by_target = Repo.find_targets_grantable_by_target
let get_default_public_title = Repo.Sql.get_default_public_title

let query_participation_history_by_contact =
  Repo.Sql.query_participation_history_by_contact
;;

let registration_possible = Repo_statistics.registration_possible
let sending_invitations = Repo_statistics.sending_invitations
let assignment_counts = Repo_statistics.assignment_counts

let find_admins_to_notify_about_invitations database_label experiment_id =
  Admin.find_all_with_permissions_on_target
    database_label
    `InvitationNotification
    (Id.to_common experiment_id)
    Guardian.Permission.[ Read ]
;;

let possible_participant_count _ = Lwt.return 0
let possible_participants _ = Lwt.return []

let smtp_auth database_label ({ smtp_auth_id; _ } : t) =
  let open Utils.Lwt_result.Infix in
  match smtp_auth_id with
  | None -> Lwt_result.return None
  | Some id -> Email.SmtpAuth.find database_label id >|+ CCOption.return
;;

let is_sessionless ({ online_experiment; _ } : t) = CCOption.is_some online_experiment
let invited_contacts_count = Repo_statistics.FilterStatistics.invited_contacts_count

module Public = struct
  include Public

  let contact_matches_filter = Repo.Public.contact_matches_filter
end

module InvitationReset = struct
  include InvitationReset

  let insert = Repo_invitation_reset.insert
  let find_by_experiment = Repo_invitation_reset.find_by_experiment
  let find_latest_by_experiment = Repo_invitation_reset.find_latest_by_experiment

  let invitations_sent_since_last_reset =
    Repo_invitation_reset.invitations_sent_since_last_reset
  ;;
end
