module Guard = Entity_guard

module System = struct
  include Entity.System

  let create database_label ?(period = default_period) () =
    let open Repo.System in
    let%lwt active_contacts = active_contacts database_label in
    let%lwt pending_contact_imports = pending_contact_imports database_label in
    let%lwt login_count = login_count database_label period in
    let%lwt sign_up_count = sign_up_count database_label period in
    let%lwt assignments_created = assignments_created database_label period in
    let%lwt invitations_sent = invitations_sent database_label period in
    let%lwt reminders_sent = reminders_sent database_label period in
    let%lwt terms_accepted_count = terms_accepted_count database_label period in
    let%lwt terms_last_changed =
      I18n.terms_and_conditions_last_updated database_label
    in
    let%lwt emails_sent = total_emails_sent database_label period in
    Lwt.return
      { active_contacts
      ; pending_contact_imports
      ; login_count
      ; sign_up_count
      ; terms_accepted_count
      ; terms_last_changed
      ; assignments_created
      ; invitations_sent
      ; reminders_sent
      ; emails_sent
      }
  ;;
end

module Experiment = struct
  include Entity.Experiment
  module Repo = Repo.Experiment

  module SentInvitations = struct
    include SentInvitations

    let create = Repo.SentInvitations.by_experiment
  end

  let create pool ({ Experiment.id; _ } as experiment) =
    let open Utils.Lwt_result.Infix in
    let%lwt registration_possible = Repo.registration_possible pool id in
    let* sending_invitations = Repo.sending_invitations pool id in
    let%lwt session_count = Repo.session_count pool id in
    let* invitations = SentInvitations.create pool experiment in
    let* not_matching_filter =
      Assignment.count_unsuitable_by pool (`Experiment id)
      ||> NotMatchingFilerCount.create
    in
    let%lwt showup_count, noshow_count, participation_count =
      Repo.assignment_counts pool id
    in
    Lwt_result.return
      { registration_possible
      ; sending_invitations
      ; session_count
      ; invitations
      ; not_matching_filter
      ; showup_count
      ; noshow_count
      ; participation_count
      }
  ;;
end
