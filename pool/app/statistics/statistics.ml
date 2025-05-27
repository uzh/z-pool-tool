include Entity
module Guard = Entity_guard

module Pool = struct
  include Pool

  let create database_label ?(period = default_period) () =
    let open Repo in
    let%lwt active_contacts = active_contacts database_label in
    let%lwt pending_contact_imports = pending_contact_imports database_label in
    let%lwt login_count = login_count database_label period in
    let%lwt sign_up_count = sign_up_count database_label period in
    let%lwt assignments_created = assignments_created database_label period in
    let%lwt invitations_sent = invitations_sent database_label period in
    let%lwt reminders_sent = reminders_sent database_label period in
    let%lwt terms_accepted_count = terms_accepted_count database_label period in
    let%lwt terms_last_changed = I18n.terms_and_conditions_last_updated database_label in
    let%lwt emails_sent = Repo.total_emails_sent database_label period in
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

module ExperimentInvitations = struct
  include ExperimentInvitations

  let create ?total_match_filter pool { Experiment.id; filter; _ } =
    let open Utils.Lwt_result.Infix in
    let%lwt invitation_resets = Experiment.InvitationReset.find_by_experiment pool id in
    let%lwt sent_since_last_reset =
      Experiment.InvitationReset.invitations_sent_since_last_reset pool id
    in
    let* total_match_filter =
      match total_match_filter with
      | Some total -> Lwt_result.return total
      | None ->
        let query = filter |> CCOption.map (fun { Filter.query; _ } -> query) in
        Filter.(
          count_filtered_contacts
            ~include_invited:true
            pool
            (Matcher (Experiment.Id.to_common id))
            query)
    in
    Lwt.return_ok { invitation_resets; sent_since_last_reset; total_match_filter }
  ;;
end

module ExperimentFilter = struct
  include ExperimentFilter

  let create pool ({ Experiment.id; filter; _ } as experiment) query =
    let open Utils.Lwt_result.Infix in
    let query =
      let open CCOption.Infix in
      query <+> (filter |> CCOption.map (fun { Filter.query; _ } -> query))
    in
    let%lwt invited_contacts_count = Experiment.invited_contacts_count pool id in
    let count_filtered_contacts ~include_invited =
      Filter.(
        count_filtered_contacts
          ~include_invited
          pool
          (Matcher (Experiment.Id.to_common id))
          query)
    in
    let* total_match_filter = count_filtered_contacts ~include_invited:true in
    let* total_uninvited_matching = count_filtered_contacts ~include_invited:false in
    let contacts_not_matching query =
      let%lwt contacts =
        Assignment.find_assigned_contacts_by_experiment pool experiment.Experiment.id
      in
      let%lwt matching =
        Lwt_list.filter_s (Filter.contact_matches_filter pool query) contacts
        ||> CCList.length
      in
      Lwt.return CCList.(length contacts - matching)
    in
    let%lwt assigned_contacts_not_matching =
      query |> CCOption.map_or ~default:(Lwt.return 0) contacts_not_matching
    in
    let* sent_invitations =
      ExperimentInvitations.create ~total_match_filter pool experiment
    in
    Lwt_result.return
      { invited_contacts_count
      ; total_match_filter
      ; total_uninvited_matching
      ; assigned_contacts_not_matching
      ; sent_invitations
      }
  ;;
end

module ExperimentOverview = struct
  include ExperimentOverview

  let create pool ({ Experiment.id; _ } as experiment) =
    let open Utils.Lwt_result.Infix in
    let%lwt registration_possible = Experiment.registration_possible pool id in
    let* sending_invitations = Experiment.sending_invitations pool id in
    let%lwt session_count = Experiment.session_count pool id in
    let* invitations = ExperimentInvitations.create pool experiment in
    let%lwt Experiment.{ show_up_count; no_show_count; participation_count } =
      Experiment.assignment_counts pool id
    in
    Lwt_result.return
      { registration_possible
      ; sending_invitations
      ; session_count
      ; invitations
      ; show_up_count
      ; no_show_count
      ; participation_count
      }
  ;;
end
