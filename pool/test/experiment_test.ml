module ExperimentCommand = Cqrs_command.Experiment_command
module Common = Pool_common
module Model = Test_utils.Model

let get_exn = Test_utils.get_or_failwith_pool_error
let database_label = Test_utils.Data.database_label

let experiment_boolean_fields =
  Experiment.boolean_fields |> CCList.map Pool_common.Message.Field.show
;;

module Data = struct
  let title = "New experiment"
  let public_title = "public_experiment_title"
  let description = "Description"

  module Filter = struct
    open Filter

    let single_query : query =
      Pred
        (Predicate.create
           Key.(Hardcoded Firstname)
           Test_utils.FilterHelper.equal
           (Single (Str "Foo")))
    ;;

    let or_query : query =
      Or
        [ Pred
            (Predicate.create
               Key.(Hardcoded Name)
               Test_utils.FilterHelper.equal
               (Single (Str "Bar")))
        ; single_query
        ]
    ;;

    let list_query : query =
      Pred
        (Predicate.create
           Key.(Hardcoded Name)
           Filter.Operator.(ListM.ContainsNone |> list)
           (Lst [ Str "foo"; Str "bar" ]))
    ;;

    let and_query : query = And [ or_query; list_query ]
    let t = create None and_query
  end

  let filter = Some Filter.t

  let experiment =
    let open CCResult in
    let open Experiment in
    let* title = title |> Title.create in
    let* public_title = public_title |> PublicTitle.create in
    let* description = description |> Description.create in
    Ok
      { id = Id.create ()
      ; title
      ; public_title
      ; description
      ; organisational_unit = None
      ; filter
      ; direct_registration_disabled =
          false |> DirectRegistrationDisabled.create
      ; registration_disabled = false |> RegistrationDisabled.create
      ; allow_uninvited_signup = false |> AllowUninvitedSignup.create
      ; experiment_type = Some Pool_common.ExperimentType.Lab
      ; session_reminder_lead_time = None
      ; created_at = Common.CreatedAt.create ()
      ; updated_at = Common.UpdatedAt.create ()
      }
  ;;
end

let create () =
  let experiment = Model.create_experiment () in
  let events = Ok [ Experiment.Created experiment |> Pool_event.experiment ] in
  let expected =
    Ok [ Experiment.Created experiment |> Pool_event.experiment ]
  in
  Test_utils.check_result expected events
;;

let create_without_title () =
  let events =
    let open CCResult.Infix in
    Pool_common.Message.Field.
      [ Title |> show, [ "" ]
      ; PublicTitle |> show, [ "public_title" ]
      ; Description |> show, [ Data.description ]
      ]
    |> Http_utils.format_request_boolean_values experiment_boolean_fields
    |> ExperimentCommand.Create.decode
    >>= ExperimentCommand.Create.handle None
  in
  let expected = Error Common.Message.(Conformist [ Field.Title, NoValue ]) in
  Test_utils.check_result expected events
;;

let update () =
  let experiment = Model.create_experiment () in
  let open CCResult.Infix in
  let events =
    Pool_common.Message.Field.
      [ Title |> show, [ Data.title ]
      ; Description |> show, [ Data.description ]
      ]
    |> ExperimentCommand.Update.decode
    >>= ExperimentCommand.Update.handle experiment None
  in
  let expected =
    Pool_common.Message.Field.
      [ Title |> show, [ Data.title ]
      ; Description |> show, [ Data.description ]
      ]
    |> ExperimentCommand.Update.decode
    >>= ExperimentCommand.Update.handle experiment None
  in
  Test_utils.check_result expected events
;;

let delete_with_sessions () =
  let experiment = Model.create_experiment () in
  let events =
    let session_count = 1234 in
    ExperimentCommand.Delete.(
      handle
        { experiment
        ; session_count
        ; mailings = []
        ; experimenters = []
        ; assistants = []
        ; templates = []
        })
  in
  let expected = Error Pool_common.Message.ExperimentSessionCountNotZero in
  Test_utils.check_result expected events
;;

let delete_with_filter () =
  let experiment = Model.create_experiment () in
  let filter = Filter.create None (Filter_test.nr_of_siblings_filter ()) in
  let experiment = Experiment.{ experiment with filter = Some filter } in
  let events =
    let session_count = 0 in
    ExperimentCommand.Delete.(
      handle
        { experiment
        ; session_count
        ; mailings = []
        ; experimenters = []
        ; assistants = []
        ; templates = []
        })
  in
  let expected =
    Ok
      [ Experiment.Deleted experiment.Experiment.id |> Pool_event.experiment
      ; Filter.Deleted filter |> Pool_event.filter
      ]
  in
  Test_utils.check_result expected events
;;

(* Integration *)

module AvailableExperiments = struct
  let contact_id = Pool_common.Id.create ()
  let experiment_id = Experiment.Id.create ()
  let session_id = Session.Id.create ()

  let list_available_experiments _ () =
    let open Utils.Lwt_result.Infix in
    let open Integration_utils in
    let%lwt contact =
      ContactRepo.create ~id:contact_id ~with_terms_accepted:true ()
    in
    let%lwt experiment = ExperimentRepo.create ~id:experiment_id () in
    let%lwt () =
      Invitation.Created ([ contact ], experiment)
      |> Pool_event.invitation
      |> Pool_event.handle_event database_label
    in
    let%lwt res =
      (* Expect the experiment to be found *)
      let public = experiment |> Model.experiment_to_public_experiment in
      Experiment.find_all_public_by_contact database_label contact
      ||> CCList.find_opt (Experiment.Public.equal public)
      ||> CCOption.is_some
    in
    let () = Alcotest.(check bool "succeeds" true res) in
    Lwt.return_unit
  ;;

  let exclude_experiment_after_registration_for_session _ () =
    let open Utils.Lwt_result.Infix in
    let%lwt experiment =
      Experiment.find database_label experiment_id
      ||> get_exn
      ||> Test_utils.Model.experiment_to_public_experiment
    in
    let%lwt contact = Contact.find database_label contact_id ||> get_exn in
    let%lwt session =
      Integration_utils.SessionRepo.create ~id:session_id experiment_id ()
    in
    let%lwt () = Integration_utils.AssignmentRepo.create session contact in
    let%lwt experiment_not_available =
      (* Expect the experiment not to be found after registration for a
         session *)
      Experiment.find_all_public_by_contact database_label contact
      ||> CCList.find_opt (Experiment.Public.equal experiment)
      ||> CCOption.is_none
    in
    let%lwt upcomming_session_found =
      (* Expect the session to be listed among the upcomming sessions *)
      Session.find_upcoming_public_by_contact
        database_label
        (Contact.id contact)
      ||> get_exn
      ||> CCList.find_opt (fun (_, upcoming, _) ->
            Session.(Id.equal upcoming.Public.id session.id))
      ||> CCOption.is_some
    in
    let res = experiment_not_available && upcomming_session_found in
    let () = Alcotest.(check bool "succeeds" true res) in
    Lwt.return_unit
  ;;

  let cancel_session _ () =
    let open Utils.Lwt_result.Infix in
    let%lwt contact = Contact.find database_label contact_id ||> get_exn in
    let%lwt session = Session.find database_label session_id ||> get_exn in
    let%lwt () =
      Session.Canceled session
      |> Pool_event.session
      |> Pool_event.handle_event database_label
    in
    let%lwt experiment_available =
      (* Expect the experiment not to be found after session cancellation to
         enable reregistration of contact *)
      Experiment.find_all_public_by_contact database_label contact
      ||> CCList.find_opt (fun experiment ->
            Experiment.(Id.equal experiment.Public.id experiment_id))
      ||> CCOption.is_some
    in
    let%lwt upcomming_session_found =
      (* Expect the session to be listed among the upcomming sessions, but to be
         marked as canceled *)
      Session.find_upcoming_public_by_contact
        database_label
        (Contact.id contact)
      ||> get_exn
      ||> CCList.find_opt (fun (_, upcoming, _) ->
            Session.(
              Id.equal upcoming.Public.id session.id
              && CCOption.is_some upcoming.Public.canceled_at))
      ||> CCOption.is_some
    in
    let res = experiment_available && upcomming_session_found in
    let () = Alcotest.(check bool "succeeds" true res) in
    Lwt.return_unit
  ;;

  let mark_assignment_as_deleted _ () =
    let open Utils.Lwt_result.Infix in
    let%lwt contact = Contact.find database_label contact_id ||> get_exn in
    let%lwt session = Session.find database_label session_id ||> get_exn in
    let%lwt () =
      let open Assignment in
      find_by_session database_label session_id
      ||> get_exn
      ||> CCList.map (fun assignment ->
            Assignment.MarkedAsDeleted assignment |> Pool_event.assignment)
      >|> Pool_event.handle_events database_label
    in
    let%lwt experiment_available =
      (* Expect the experiment not to be found after session cancellation to
         enable reregistration of contact *)
      Experiment.find_all_public_by_contact database_label contact
      ||> CCList.find_opt (fun experiment ->
            Experiment.(Id.equal experiment.Public.id experiment_id))
      ||> CCOption.is_some
    in
    let%lwt upcomming_session_not_found =
      (* Expect the session not to be listed, as the assignments are marked as
         deleted *)
      Session.find_upcoming_public_by_contact
        database_label
        (Contact.id contact)
      ||> get_exn
      ||> CCList.find_opt (fun (_, upcoming, _) ->
            Session.(Id.equal upcoming.Public.id session.id))
      ||> CCOption.is_none
    in
    let res = experiment_available && upcomming_session_not_found in
    let () = Alcotest.(check bool "succeeds" true res) in
    Lwt.return_unit
  ;;
end
