open Test_utils

module AssignmentRepo = struct
  let create session contact =
    Assignment.(Created { contact; session_id = session.Session.id })
    |> Pool_event.assignment
    |> Pool_event.handle_event Data.database_label
  ;;
end

module ContactRepo = struct
  let create ?id ?(with_terms_accepted = false) () =
    let open Utils.Lwt_result.Infix in
    let contact = Model.create_contact ?id ~with_terms_accepted () in
    let open Contact in
    let confirm = [ Verified contact; EmailVerified contact ] in
    let%lwt () =
      [ Created
          { user_id = id contact
          ; email = email_address contact
          ; password =
              contact.user.Sihl_user.password
              |> Pool_user.Password.create
              |> get_or_failwith_pool_error
          ; firstname = firstname contact
          ; lastname = lastname contact
          ; terms_accepted_at = None
          ; language = contact.language
          }
      ]
      @ confirm
      |> Lwt_list.iter_s (handle_event Data.database_label)
    in
    contact |> id |> find Data.database_label ||> get_or_failwith_pool_error
  ;;
end

module AdminRepo = struct
  let create ?id ?email () =
    let open Pool_user in
    let admin_id = id |> CCOption.value ~default:(Admin.Id.create ()) in
    let open Admin in
    let open Utils.Lwt_result.Infix in
    let tags = Pool_database.Logger.Tags.create Data.database_label in
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
      Admin.
        { id = Some admin_id
        ; lastname = Lastname.of_string "Bar"
        ; firstname = Firstname.of_string "Foo"
        ; password = Password.create "Password1!" |> CCResult.get_exn
        ; email
        ; roles = None
        }
    in
    let%lwt () =
      [ Created admin ]
      |> Lwt_list.iter_s (handle_event ~tags Data.database_label)
    in
    admin_id |> find Data.database_label ||> get_or_failwith_pool_error
  ;;
end

module ExperimentRepo = struct
  let create ?(id = Experiment.Id.create ()) () =
    let experiment = Model.create_experiment ~id () in
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

module WaitingListRepo = struct
  let create experiment contact () =
    let open Utils.Lwt_result.Infix in
    let%lwt () =
      Waiting_list.(Created { experiment; contact })
      |> Pool_event.waiting_list
      |> Pool_event.handle_event Data.database_label
    in
    Waiting_list.find_by_contact_and_experiment
      Data.database_label
      contact
      experiment
    ||> get_or_failwith_pool_error
  ;;
end

module SessionRepo = struct
  let create ?id ?location ?follow_up_to ?start experiment_id () =
    let%lwt location =
      location |> CCOption.map_or ~default:(LocationRepo.create ()) Lwt.return
    in
    let session = Model.create_session ?id ~location ?follow_up_to ?start () in
    let%lwt () =
      Session.(Created (session, experiment_id))
      |> Pool_event.session
      |> Pool_event.handle_event Data.database_label
    in
    Lwt.return session
  ;;
end
