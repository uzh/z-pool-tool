open Test_utils

module ContactRepo = struct
  let create ?id ?(with_terms_accepted = false) () =
    let open Utils.Lwt_result.Infix in
    let contact = Model.create_contact ?id ~with_terms_accepted () in
    let verified =
      if contact.Contact.user.Sihl_user.confirmed
      then Contact.[ Verified contact ]
      else []
    in
    let%lwt () =
      [ Contact.(
          Created
            { user_id = Contact.id contact
            ; email = Contact.email_address contact
            ; password =
                contact.Contact.user.Sihl_user.password
                |> Pool_user.Password.create
                |> get_or_failwith_pool_error
            ; firstname = Contact.firstname contact
            ; lastname = Contact.lastname contact
            ; terms_accepted_at = None
            ; language = contact.language
            })
      ]
      @ verified
      |> Lwt_list.iter_s (Contact.handle_event Data.database_label)
    in
    contact
    |> Contact.id
    |> Contact.find Data.database_label
    ||> get_or_failwith_pool_error
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

module SessionRepo = struct
  let create
    ?(id = Pool_common.Id.create ())
    ?follow_up_to
    ?location
    ~experiment_id
    ~start
    ()
    =
    let session =
      Model.(create_session ~id ?location ?follow_up_to ~start ())
    in
    Session.Created (session, experiment_id)
    |> Pool_event.session
    |> Pool_event.handle_event Data.database_label
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
