open Utils.Lwt_result.Infix
open Alcotest

let make_filter pool user experiment title =
  let open Filter in
  let f =
    Pred
      (Predicate.create
         Key.(Hardcoded Firstname)
         Operator.(Equality Equality.Equal)
         (Single (Str title)))
    |> create None
  in
  [ Filter.Created f |> Pool_event.filter
  ; Experiment.(Updated (experiment, { experiment with filter = Some f }))
    |> Pool_event.experiment
  ]
  |> Pool_event.handle_events pool user
  >|> fun () ->
  Experiment.find pool experiment.Experiment.id ||> Test_utils.get_or_failwith
;;

let invitation_statistics _ () =
  let open Integration_utils in
  let open Experiment in
  let open Statistics in
  let n_contacts = 10 in
  let testable = ExperimentInvitations.(testable pp equal) in
  let pool = Test_utils.Data.database_label in
  let%lwt current_user = create_admin_user () in
  let title = Pool_common.Id.(create () |> value) in
  let%lwt experiment = ExperimentRepo.create ~title () in
  let%lwt experiment = make_filter pool current_user experiment title in
  let%lwt (_ : Contact.t list) =
    Lwt_list.fold_left_s
      (fun acc _ ->
         let%lwt contact =
           ContactRepo.create ~firstname:(Pool_user.Firstname.of_string title) ()
         in
         Lwt.return (contact :: acc))
      []
      (List.init n_contacts (fun _ -> ()))
  in
  let invite_contacts n =
    let%lwt mailing = MailingRepo.create experiment.id in
    Matcher.events_of_mailings [ pool, [ mailing, n ] ]
    ||> CCList.hd
    >|> fun (pool, events) -> Pool_event.handle_events pool current_user events
  in
  let check_invitations msg expected =
    let%lwt result =
      ExperimentInvitations.create pool experiment ||> Test_utils.get_or_failwith
    in
    let msg = Format.asprintf "Invitations statistics: %s" msg in
    check testable msg expected result |> Lwt.return
  in
  let check_total_invited msg expected =
    let%lwt result = Experiment.invited_contacts_count pool experiment.id in
    let msg = Format.asprintf "Total invited count: %s" msg in
    check int msg expected result |> Lwt.return
  in
  (* Invite 5 contacts *)
  let%lwt () = invite_contacts 5 in
  let expected =
    ExperimentInvitations.
      { total_match_filter = n_contacts
      ; invitation_resets = []
      ; sent_since_last_reset = 5
      }
  in
  let%lwt () = check_invitations "5 contacts invited" expected in
  let%lwt () = check_total_invited "5 contacts invited" 5 in
  (* Invite 10 more contacts *)
  let%lwt () = invite_contacts 10 in
  let expected = ExperimentInvitations.{ expected with sent_since_last_reset = 10 } in
  let%lwt () = check_invitations "10 more contacts invited" expected in
  let%lwt () = check_total_invited "10 more contacts invited" 10 in
  (* Reset invitations *)
  let%lwt () = Experiment.ResetInvitations experiment |> Experiment.handle_event pool in
  let expected =
    let invitation_reset =
      InvitationReset.
        { created_at = Pool_common.CreatedAt.create_now ()
        ; iteration = 1
        ; contacts_matching_filter = n_contacts
        ; invitations_sent = n_contacts
        }
    in
    ExperimentInvitations.
      { expected with
        sent_since_last_reset = 0
      ; invitation_resets = [ invitation_reset ]
      }
  in
  let%lwt () = check_invitations "Reset invitations" expected in
  Unix.sleep 1;
  (* Invite 5 more contacts *)
  let%lwt () = invite_contacts 5 in
  let expected = ExperimentInvitations.{ expected with sent_since_last_reset = 5 } in
  let%lwt () = check_invitations "Invite 5 more contacts after reset" expected in
  (* Reset invitations again *)
  let%lwt () = Experiment.ResetInvitations experiment |> Experiment.handle_event pool in
  let expected =
    let invitation_reset =
      InvitationReset.
        { created_at = Pool_common.CreatedAt.create_now ()
        ; iteration = 2
        ; contacts_matching_filter = n_contacts
        ; invitations_sent = 5
        }
    in
    ExperimentInvitations.
      { expected with
        sent_since_last_reset = 0
      ; invitation_resets = expected.invitation_resets @ [ invitation_reset ]
      }
  in
  let%lwt () = check_invitations "Reset invitations again" expected in
  Lwt.return ()
;;
