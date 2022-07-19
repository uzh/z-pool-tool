module User = Pool_user
module Id = Pool_common.Id

let get_or_failwith = Pool_common.Utils.get_or_failwith

let admins db_pool =
  let data =
    [ "The", "One", "admin@example.com", `Operator
    ; "engineering", "admin", "engineering@econ.uzh.ch", `Operator
    ; "Scooby", "Doo", "assistant@econ.uzh.ch", `Assistant
    ; "Winnie", "Pooh", "experimenter@econ.uzh.ch", `Experimenter
    ]
  in
  let ctx = Pool_tenant.to_ctx db_pool in
  let password =
    Sys.getenv_opt "POOL_ADMIN_DEFAULT_PASSWORD"
    |> CCOption.value ~default:"admin"
  in
  Lwt_list.iter_s
    (fun (given_name, name, email, role) ->
      let%lwt user = Service.User.find_by_email_opt ~ctx email in
      match user with
      | None ->
        let%lwt user =
          Service.User.create_admin ~ctx ~name ~given_name ~password email
        in
        let person = Admin.create_person user in
        let%lwt () =
          match role with
          | `Assistant -> Admin.insert db_pool (Admin.Assistant person)
          | `Experimenter -> Admin.insert db_pool (Admin.Experimenter person)
          | `Recruiter -> Admin.insert db_pool (Admin.Recruiter person)
          | `LocationManager ->
            Admin.insert db_pool (Admin.LocationManager person)
          | `Operator -> Admin.insert db_pool (Admin.Operator person)
        in
        Lwt.return_unit
      | Some _ ->
        Logs.debug (fun m -> m "%s" "Admin user already exists");
        Lwt.return_unit)
    data
;;

let contacts db_pool =
  let users =
    [ ( Id.create ()
      , "Hansruedi"
      , "Rüdisüli"
      , "one@test.com"
      , Contact.RecruitmentChannel.Friend
      , Some Pool_common.Language.De
      , Some (Ptime_clock.now ())
      , false
      , false
      , true )
    ; ( Id.create ()
      , "Jane"
      , "Doe"
      , "two@test.com"
      , Contact.RecruitmentChannel.Online
      , Some Pool_common.Language.En
      , Some (Ptime_clock.now ())
      , false
      , false
      , true )
    ; ( Id.create ()
      , "John"
      , "Dorrian"
      , "three@mail.com"
      , Contact.RecruitmentChannel.Lecture
      , Some Pool_common.Language.De
      , Some (Ptime_clock.now ())
      , true
      , false
      , true )
    ; ( Id.create ()
      , "Kevin"
      , "McCallistor"
      , "four@mail.com"
      , Contact.RecruitmentChannel.Mailing
      , Some Pool_common.Language.En
      , Some (Ptime_clock.now ())
      , true
      , false
      , true )
    ; ( Id.create ()
      , "Hello"
      , "Kitty"
      , "five@mail.com"
      , Contact.RecruitmentChannel.Online
      , None
      , Some (Ptime_clock.now ())
      , true
      , true
      , true )
    ; ( Id.create ()
      , "Dr."
      , "Murphy"
      , "six@mail.com"
      , Contact.RecruitmentChannel.Friend
      , None
      , Some (Ptime_clock.now ())
      , true
      , true
      , false )
    ; ( Id.create ()
      , "Mr."
      , "Do not accept terms"
      , "seven@mail.com"
      , Contact.RecruitmentChannel.Friend
      , Some Pool_common.Language.En
      , None
      , true
      , true
      , false )
    ]
  in
  let password =
    Sys.getenv_opt "POOL_USER_DEFAULT_PASSWORD"
    |> CCOption.value ~default:"user"
  in
  let%lwt () =
    let open Lwt.Infix in
    Lwt_list.fold_left_s
      (fun contacts
           ( user_id
           , firstname
           , lastname
           , email
           , recruitment_channel
           , language
           , terms_accepted_at
           , _
           , _
           , _ ) ->
        let ctx = Pool_tenant.to_ctx db_pool in
        let%lwt user = Service.User.find_by_email_opt ~ctx email in
        match user with
        | None ->
          [ Contact.Created
              { Contact.user_id
              ; email = email |> User.EmailAddress.of_string
              ; password =
                  password
                  |> User.Password.create
                  |> Pool_common.Utils.get_or_failwith
              ; firstname = firstname |> User.Firstname.of_string
              ; lastname = lastname |> User.Lastname.of_string
              ; recruitment_channel
              ; terms_accepted_at = User.TermsAccepted.create terms_accepted_at
              ; language
              }
          ]
          @ contacts
          |> Lwt.return
        | Some { Sihl_user.id; _ } ->
          Logs.debug (fun m ->
              m
                "Contact already exists (%s): %s"
                (db_pool |> Pool_database.Label.value)
                id);
          contacts |> Lwt.return)
      []
      users
    >>= Lwt_list.iter_s (Contact.handle_event db_pool)
  in
  let open Lwt.Infix in
  Lwt_list.fold_left_s
    (fun contacts (user_id, _, _, _, _, _, _, paused, disabled, verified) ->
      let%lwt contact = Contact.find db_pool user_id in
      match contact with
      | Ok contact ->
        [ Contact.PausedUpdated (contact, paused |> User.Paused.create) ]
        @ (if disabled then [ Contact.Disabled contact ] else [])
        @ (if verified then [ Contact.EmailVerified contact ] else [])
        @ contacts
        |> Lwt.return
      | Error err ->
        let _ = Pool_common.Utils.with_log_error ~level:Logs.Debug err in
        contacts |> Lwt.return)
    []
    users
  >>= Lwt_list.iter_s (Contact.handle_event db_pool)
;;
