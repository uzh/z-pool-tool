open Test_utils

let default_current_user = Model.create_admin ()

module AnnouncementRepo = struct
  let create
        ?(current_user = default_current_user)
        ?id
        ?start_at
        ?end_at
        ?show_to_admins
        ?show_to_contacts
        tenant_ids
    =
    let announcement =
      Test_utils.Model.create_announcement
        ?id
        ?start_at
        ?end_at
        ?show_to_admins
        ?show_to_contacts
        ()
    in
    let%lwt () =
      Announcement.Created (announcement, tenant_ids)
      |> Pool_event.announcement
      |> Pool_event.handle_event Database.Pool.Root.label current_user
    in
    Lwt.return announcement
  ;;
end

module AssignmentRepo = struct
  let create ?(current_user = default_current_user) ?id session contact =
    let open Utils.Lwt_result.Infix in
    let assignment = Assignment.create ?id contact in
    let%lwt () =
      Assignment.(Created (assignment, session.Session.id))
      |> Pool_event.assignment
      |> Pool_event.handle_event Data.database_label current_user
    in
    Assignment.find_by_contact_and_experiment
      Data.database_label
      session.Session.experiment.Experiment.id
      contact
    ||> CCList.find (fun ({ Session.id; _ }, _) -> Session.Id.equal id session.Session.id)
    ||> snd
  ;;
end

module ContactRepo = struct
  let create
        ?(current_user = default_current_user)
        ?firstname
        ?id
        ?lastname
        ?language
        ?(with_terms_accepted = false)
        ?(password = Model.password)
        ()
    =
    let open Utils.Lwt_result.Infix in
    let contact =
      Model.create_contact ?id ?firstname ?lastname ?language ~with_terms_accepted ()
    in
    let open Contact in
    let confirm = [ EmailVerified contact ] in
    let%lwt () =
      [ Created
          { user_id = id contact
          ; email = email_address contact
          ; password
          ; firstname = firstname contact
          ; lastname = lastname contact
          ; terms_accepted_at = None
          ; language = contact.language
          }
      ]
      @ confirm
      |> Pool_event.(map contact)
      |> Pool_event.handle_events Data.database_label current_user
    in
    contact |> id |> find Data.database_label ||> get_or_failwith
  ;;
end

module CustomFieldRepo = struct
  let pool = Data.database_label

  open Custom_field_utils

  let create name encoder =
    let field = create_custom_field name encoder in
    let%lwt () =
      [ save_custom_field field; Custom_field.Published field |> Pool_event.custom_field ]
      |> Pool_event.handle_events pool default_current_user
    in
    Custom_field.find pool (Custom_field.id field) |> Lwt.map get_or_failwith
  ;;
end

module AdminRepo = struct
  open Pool_user

  let create ?(current_user = default_current_user) ?id ?email () =
    let admin_id = id |> CCOption.value ~default:(Admin.Id.create ()) in
    let open Admin in
    let open Utils.Lwt_result.Infix in
    let tags = Database.Logger.Tags.create Data.database_label in
    let email =
      email
      |> CCOption.value
           ~default:
             (Format.asprintf "test+%s@econ.uzh.ch" Pool_common.Id.(create () |> value))
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
      [ Created admin |> Pool_event.admin ]
      |> Pool_event.handle_events ~tags Data.database_label current_user
    in
    admin_id |> find Data.database_label ||> get_or_failwith
  ;;
end

module ExperimentRepo = struct
  let create
        ?(current_user = default_current_user)
        ?(id = Experiment.Id.create ())
        ?title
        ?online_experiment
        ()
    =
    let experiment = Model.create_experiment ~id ?title ?online_experiment () in
    let%lwt () =
      Experiment.Created experiment
      |> Pool_event.experiment
      |> Pool_event.handle_event Data.database_label current_user
    in
    experiment |> Lwt.return
  ;;
end

module LocationRepo = struct
  let create ?(current_user = default_current_user) ?(id = Pool_location.Id.create ()) () =
    let location = Model.create_location ~id () in
    let%lwt () =
      Pool_location.Created location
      |> Pool_event.pool_location
      |> Pool_event.handle_event Data.database_label current_user
    in
    location |> Lwt.return
  ;;
end

module MailingRepo = struct
  let create
        ?(id = Mailing.Id.create ())
        ?start
        ?duration
        ?distribution
        ?limit
        experiment_id
    =
    let mailing = Model.create_mailing ~id ?start ?duration ?distribution ?limit () in
    let%lwt () =
      Mailing.(Created (mailing, experiment_id) |> handle_event Data.database_label)
    in
    Mailing.find Data.database_label id |> Lwt.map get_or_failwith
  ;;
end

module WaitingListRepo = struct
  (* TODO: Is there a case where admins create waiting list entries? Or will it always be
     the contact *)
  let create experiment contact () =
    let%lwt () =
      Waiting_list.Created { Waiting_list.experiment; contact }
      |> Pool_event.waiting_list
      |> Pool_event.handle_event Data.database_label (Pool_context.Contact contact)
    in
    experiment
    |> Experiment.Public.id
    |> Waiting_list.find_by_contact_and_experiment Data.database_label contact
  ;;
end

module SessionRepo = struct
  let create
        ?(current_user = default_current_user)
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
      |> Pool_event.handle_event Data.database_label current_user
    in
    (* To compare the start time it is required to read the session from the database again *)
    Session.find Data.database_label session.Session.id |> Lwt.map get_or_failwith
  ;;
end

module TagRepo = struct
  let create ?(id = Tags.Id.create ()) ?(name = "Tag") model =
    let open Tags in
    let tag = Tags.create ~id (Title.of_string name) model |> get_or_failwith in
    let%lwt () = Tags.(Created tag |> handle_event Data.database_label) in
    Tags.find Data.database_label id |> Lwt.map get_or_failwith
  ;;
end

module TimeWindowRepo = struct
  let create ?(current_user = default_current_user) ?id start duration experiment () =
    let time_window = Time_window.create ?id start duration experiment in
    let%lwt () =
      Time_window.(Created time_window)
      |> Pool_event.time_window
      |> Pool_event.handle_event Data.database_label current_user
    in
    Lwt.return time_window
  ;;
end

let create_admin_user () = AdminRepo.create () |> Lwt.map Pool_context.admin
let create_contact_user () = ContactRepo.create () |> Lwt.map Pool_context.contact

let create_admin_actor () =
  let open Utils.Lwt_result.Infix in
  AdminRepo.create ()
  ||> Pool_context.admin
  >|> Pool_context.Utils.find_authorizable Test_utils.Data.database_label
  ||> Test_utils.get_or_failwith
;;

let create_contact_actor () =
  let open Utils.Lwt_result.Infix in
  ContactRepo.create ()
  ||> Pool_context.contact
  >|> Pool_context.Utils.find_authorizable Test_utils.Data.database_label
  ||> Test_utils.get_or_failwith
;;
