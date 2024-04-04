open CCFun
module ExperimentCommand = Cqrs_command.Experiment_command
module Model = Test_utils.Model

let get_exn = Test_utils.get_or_failwith
let database_label = Test_utils.Data.database_label

let experiment_boolean_fields =
  Experiment.boolean_fields |> CCList.map Pool_common.Message.Field.show
;;

let boolean_fields =
  Experiment.boolean_fields |> CCList.map Pool_common.Message.Field.show
;;

module Data = struct
  let organisational_unit = Test_utils.Model.create_organisational_unit ()

  let contact_person =
    let open Pool_context in
    Test_utils.Model.create_admin ()
    |> function
    | Admin admin -> admin
    | Contact _ | Guest -> failwith "Invalid admin"
  ;;

  let title = "New experiment"
  let public_title = "public_experiment_title"
  let internal_description = "Internal Description"
  let public_description = "Public Description"
  let language = Pool_common.Language.En
  let cost_center = "cost_center"
  let direct_registration_disabled = "false"
  let registration_disabled = "false"
  let allow_uninvited_signup = "false"
  let external_data_required = "false"
  let show_external_data_id_links = "false"
  let experiment_type = Pool_common.ExperimentType.(show Lab)

  let urlencoded =
    Pool_common.Message.
      [ Field.(show Title), [ title ]
      ; Field.(show PublicTitle), [ public_title ]
      ; Field.(show InternalDescription), [ internal_description ]
      ; Field.(show PublicDescription), [ public_description ]
      ; Field.(show Language), [ Pool_common.Language.show language ]
      ; Field.(show CostCenter), [ cost_center ]
      ; ( Field.(show DirectRegistrationDisabled)
        , [ direct_registration_disabled ] )
      ; Field.(show RegistrationDisabled), [ registration_disabled ]
      ; Field.(show AllowUninvitedSignup), [ allow_uninvited_signup ]
      ; Field.(show ExternalDataRequired), [ external_data_required ]
      ; Field.(show ShowExteralDataIdLinks), [ show_external_data_id_links ]
      ; Field.(show ExperimentType), [ experiment_type ]
      ]
  ;;

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
    let to_bool = Utils.Bool.of_string in
    let* title = title |> Experiment.Title.create in
    let* public_title = public_title |> Experiment.PublicTitle.create in
    let* internal_description =
      internal_description |> Experiment.InternalDescription.create
    in
    let* public_description =
      public_description |> Experiment.PublicDescription.create
    in
    Experiment.create
      ~cost_center:(cost_center |> Experiment.CostCenter.of_string)
      ~internal_description
      ~public_description
      ~language
      ~experiment_type:(experiment_type |> Pool_common.ExperimentType.read)
      ?filter
      title
      public_title
      (direct_registration_disabled
       |> to_bool
       |> Experiment.DirectRegistrationDisabled.create)
      (registration_disabled
       |> to_bool
       |> Experiment.RegistrationDisabled.create)
      (allow_uninvited_signup
       |> to_bool
       |> Experiment.AllowUninvitedSignup.create)
      (external_data_required
       |> to_bool
       |> Experiment.ExternalDataRequired.create)
      (show_external_data_id_links
       |> to_bool
       |> Experiment.ShowExternalDataIdLinks.create)
  ;;
end

let handle_update ?organisational_unit ?smtp_auth experiment =
  ExperimentCommand.Update.handle experiment organisational_unit smtp_auth
;;

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
      ; InternalDescription |> show, [ Data.internal_description ]
      ]
    |> Http_utils.format_request_boolean_values experiment_boolean_fields
    |> ExperimentCommand.Create.decode
    >>= ExperimentCommand.Create.handle
  in
  let expected =
    Error Pool_common.Message.(Conformist [ Field.Title, NoValue ])
  in
  Test_utils.check_result expected events
;;

let update () =
  let experiment = Data.experiment |> get_exn in
  let open CCResult.Infix in
  let events =
    Data.urlencoded
    |> Http_utils.format_request_boolean_values boolean_fields
    |> ExperimentCommand.Update.decode
    >>= handle_update experiment
  in
  let expected =
    Ok [ Experiment.Updated experiment |> Pool_event.experiment ]
  in
  Test_utils.check_result expected events
;;

let update_add_ou_and_contact_person () =
  let experiment = Data.experiment |> get_exn in
  let open CCResult.Infix in
  let events =
    Data.urlencoded
    |> Http_utils.format_request_boolean_values boolean_fields
    |> ExperimentCommand.Update.decode
    >>= handle_update ~organisational_unit:Data.organisational_unit experiment
  in
  let expected =
    Ok
      Experiment.
        [ Updated
            { experiment with
              organisational_unit = Some Data.organisational_unit
            }
          |> Pool_event.experiment
        ]
  in
  Test_utils.check_result expected events
;;

let update_remove_ou () =
  let experiment = Data.experiment |> get_exn in
  let experiment =
    Experiment.
      { experiment with organisational_unit = Some Data.organisational_unit }
  in
  let open CCResult.Infix in
  let events =
    Data.urlencoded
    |> Http_utils.format_request_boolean_values boolean_fields
    |> ExperimentCommand.Update.decode
    >>= handle_update experiment
  in
  let expected =
    Ok
      Experiment.
        [ Updated { experiment with organisational_unit = None }
          |> Pool_event.experiment
        ]
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
  let experiment = { experiment with Experiment.filter = Some filter } in
  let system_event_id = System_event.Id.create () in
  let events =
    let session_count = 0 in
    ExperimentCommand.Delete.(
      handle
        ~system_event_id
        { experiment
        ; session_count
        ; mailings = []
        ; experimenters = []
        ; assistants = []
        ; templates = []
        })
  in
  let expected =
    let system_event =
      System_event.(
        Job.GuardianCacheCleared |> create ~id:system_event_id |> created)
    in
    Ok
      [ Experiment.Deleted experiment.Experiment.id |> Pool_event.experiment
      ; Filter.Deleted filter |> Pool_event.filter
      ; system_event |> Pool_event.system_event
      ]
  in
  Test_utils.check_result expected events
;;

(* Integration *)

let autofill_public_title _ () =
  let open Utils.Lwt_result.Infix in
  let open Experiment in
  let without_title =
    let experiment = Model.create_experiment () in
    { experiment with public_title = PublicTitle.placeholder }
  in
  let with_title = Model.create_experiment () in
  let%lwt () =
    [ without_title; with_title ]
    |> CCList.map (created %> Pool_event.experiment)
    |> Pool_event.handle_events database_label
  in
  let find id = Experiment.find database_label id ||> get_exn in
  let%lwt without_title_persisted = find without_title.id in
  let%lwt with_title_persisted = find with_title.id in
  let () =
    Alcotest.(
      check
        bool
        "succeeds"
        false
        PublicTitle.(equal without_title_persisted.public_title placeholder))
  in
  let () =
    Alcotest.(
      check
        bool
        "succeeds"
        true
        PublicTitle.(
          equal with_title_persisted.public_title with_title.public_title))
  in
  Lwt.return_unit
;;

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
      Invitation.(
        Created { contacts = [ contact ]; mailing = None; experiment })
      |> Pool_event.invitation
      |> Pool_event.handle_event database_label
    in
    let%lwt res =
      (* Expect the experiment to be found *)
      let public = experiment |> Experiment.to_public in
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
      Experiment.find database_label experiment_id ||> get_exn
    in
    let%lwt contact = Contact.find database_label contact_id ||> get_exn in
    let%lwt session =
      Integration_utils.SessionRepo.create ~id:session_id experiment ()
    in
    let%lwt (_ : Assignment.t) =
      Integration_utils.AssignmentRepo.create session contact
    in
    let%lwt experiment_not_available =
      (* Expect the experiment not to be found after registration for a
         session *)
      let open Experiment in
      find_all_public_by_contact database_label contact
      ||> CCList.find_opt (fun public ->
        Id.equal (Public.id public) experiment.id)
      ||> CCOption.is_none
    in
    let%lwt upcoming_session_found =
      (* Expect the session to be listed among the upcoming sessions *)
      Session.find_upcoming_public_by_contact
        database_label
        (Contact.id contact)
      ||> get_exn
      ||> CCList.find_opt (fun (_, upcoming, _) ->
        Session.(Id.equal upcoming.Public.id session.id))
      ||> CCOption.is_some
    in
    let res = experiment_not_available && upcoming_session_found in
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
      let open Experiment in
      find_all_public_by_contact database_label contact
      ||> CCList.find_opt (Public.id %> Id.equal experiment_id)
      ||> CCOption.is_some
    in
    let%lwt upcoming_session_found =
      (* Expect the session to be listed among the upcoming sessions, but to be
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
    let res = experiment_available && upcoming_session_found in
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
      ||> CCList.map (fun assignment ->
        Assignment.MarkedAsDeleted assignment |> Pool_event.assignment)
      >|> Pool_event.handle_events database_label
    in
    let%lwt experiment_available =
      (* Expect the experiment not to be found after session cancellation to
         enable reregistration of contact *)
      let open Experiment in
      find_all_public_by_contact database_label contact
      ||> CCList.find_opt (Public.id %> Id.equal experiment_id)
      ||> CCOption.is_some
    in
    let%lwt upcoming_session_not_found =
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
    let res = experiment_available && upcoming_session_not_found in
    let () = Alcotest.(check bool "succeeds" true res) in
    Lwt.return_unit
  ;;
end
