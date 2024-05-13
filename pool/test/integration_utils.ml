open Test_utils

module AssignmentRepo = struct
  let create ?id session contact =
    let assignment = Assignment.create ?id contact in
    let%lwt () =
      Assignment.(Created (assignment, session.Session.id))
      |> Pool_event.assignment
      |> Pool_event.handle_event Data.database_label
    in
    Lwt.return assignment
  ;;
end

module ContactRepo = struct
  let create ?id ?lastname ?language ?(with_terms_accepted = false) () =
    let open Utils.Lwt_result.Infix in
    let contact =
      Model.create_contact ?id ?lastname ?language ~with_terms_accepted ()
    in
    let open Contact in
    let confirm = [ Verified contact; EmailVerified contact ] in
    let%lwt () =
      [ Created
          { user_id = id contact
          ; email = email_address contact
          ; password = Model.password
          ; firstname = firstname contact
          ; lastname = lastname contact
          ; terms_accepted_at = None
          ; language = contact.language
          }
      ]
      @ confirm
      |> Lwt_list.iter_s (handle_event Data.database_label)
    in
    contact |> id |> find Data.database_label ||> get_or_failwith
  ;;
end

module AdminRepo = struct
  open Pool_user

  let create ?id ?email () =
    let admin_id = id |> CCOption.value ~default:(Admin.Id.create ()) in
    let open Admin in
    let open Utils.Lwt_result.Infix in
    let tags = Database.Logger.Tags.create Data.database_label in
    let email =
      email
      |> CCOption.value
           ~default:
             (Format.asprintf
                "test+%s@econ.uzh.ch"
                (Uuidm.v `V4 |> Uuidm.to_string))
      |> Pool_user.EmailAddress.of_string
    in
    let admin =
      { Admin.id = Some admin_id
      ; lastname = Lastname.of_string "Bar"
      ; firstname = Firstname.of_string "Foo"
      ; password = Password.Plain.create "Password1!"
      ; email
      ; roles = []
      }
    in
    let%lwt () =
      [ Created admin ]
      |> Lwt_list.iter_s (handle_event ~tags Data.database_label)
    in
    admin_id |> find Data.database_label ||> get_or_failwith
  ;;
end

module ExperimentRepo = struct
  let create ?(id = Experiment.Id.create ()) ?title ?online_experiment () =
    let experiment = Model.create_experiment ~id ?title ?online_experiment () in
    let%lwt () =
      Experiment.Created experiment
      |> Pool_event.experiment
      |> Pool_event.handle_event Data.database_label
    in
    experiment |> Lwt.return
  ;;
end

module LocationRepo = struct
  let create ?(id = Pool_location.Id.create ()) () =
    let location = Model.create_location ~id () in
    let%lwt () =
      Pool_location.Created location
      |> Pool_event.pool_location
      |> Pool_event.handle_event Data.database_label
    in
    location |> Lwt.return
  ;;
end

module MailingRepo = struct
  let create ?(id = Mailing.Id.create ()) ?limit experiment_id =
    let mailing = Model.create_mailing ~id ?limit () in
    let%lwt () =
      Mailing.(
        Created (mailing, experiment_id) |> handle_event Data.database_label)
    in
    Mailing.find Data.database_label id |> Lwt.map get_or_failwith
  ;;
end

module WaitingListRepo = struct
  let create experiment contact () =
    let%lwt () =
      Waiting_list.Created { Waiting_list.experiment; contact }
      |> Pool_event.waiting_list
      |> Pool_event.handle_event Data.database_label
    in
    experiment
    |> Experiment.Public.id
    |> Waiting_list.find_by_contact_and_experiment Data.database_label contact
  ;;
end

module SessionRepo = struct
  let create
    ?id
    ?location
    ?follow_up_to
    ?start
    ?duration
    ?email_reminder_sent_at
    experiment
    ()
    =
    let%lwt location =
      location |> CCOption.map_or ~default:(LocationRepo.create ()) Lwt.return
    in
    let session =
      Model.create_session
        ?id
        ~location
        ?follow_up_to
        ?start
        ?duration
        ?email_reminder_sent_at
        ~experiment
        ()
    in
    let%lwt () =
      Session.(Created session)
      |> Pool_event.session
      |> Pool_event.handle_event Data.database_label
    in
    Lwt.return session
  ;;
end

module TimeWindowRepo = struct
  let create ?id start duration experiment () =
    let time_window = Time_window.create ?id start duration experiment in
    let%lwt () =
      Time_window.(Created time_window)
      |> Pool_event.time_window
      |> Pool_event.handle_event Data.database_label
    in
    Lwt.return time_window
  ;;
end
