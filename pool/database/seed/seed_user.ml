module User = Pool_user
module Id = Pool_common.Id

let get_or_failwith = Pool_common.Utils.get_or_failwith

let admins db_pool () =
  let data =
    [ "The", "One", "admin@example.com", `Operator
    ; "engineering", "admin", "engineering@econ.uzh.ch", `Operator
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

let participants db_pool () =
  let users =
    [ ( Id.create ()
      , "Hansruedi"
      , "Rüdisüli"
      , "one@test.com"
      , Participant.RecruitmentChannel.Friend
      , Some Pool_common.Language.De
      , Some (Ptime_clock.now ())
      , false
      , false
      , Some (Ptime_clock.now ()) )
    ; ( Id.create ()
      , "Jane"
      , "Doe"
      , "two@test.com"
      , Participant.RecruitmentChannel.Online
      , Some Pool_common.Language.En
      , Some (Ptime_clock.now ())
      , false
      , false
      , None )
    ; ( Id.create ()
      , "John"
      , "Dorrian"
      , "three@mail.com"
      , Participant.RecruitmentChannel.Lecture
      , Some Pool_common.Language.De
      , Some (Ptime_clock.now ())
      , true
      , false
      , Some (Ptime_clock.now ()) )
    ; ( Id.create ()
      , "Kevin"
      , "McCallistor"
      , "four@mail.com"
      , Participant.RecruitmentChannel.Mailing
      , Some Pool_common.Language.En
      , Some (Ptime_clock.now ())
      , true
      , false
      , None )
    ; ( Id.create ()
      , "Hello"
      , "Kitty"
      , "five@mail.com"
      , Participant.RecruitmentChannel.Online
      , None
      , Some (Ptime_clock.now ())
      , true
      , true
      , Some (Ptime_clock.now ()) )
    ; ( Id.create ()
      , "Dr."
      , "Murphy"
      , "six@mail.com"
      , Participant.RecruitmentChannel.Friend
      , None
      , Some (Ptime_clock.now ())
      , true
      , true
      , None )
    ; ( Id.create ()
      , "Mr."
      , "Do not accept terms"
      , "six@mail.com"
      , Participant.RecruitmentChannel.Friend
      , Some Pool_common.Language.En
      , None
      , true
      , true
      , None )
    ]
  in
  let password =
    Sys.getenv_opt "POOL_USER_DEFAULT_PASSWORD"
    |> CCOption.value ~default:"user"
  in
  Lwt_list.iter_s
    (fun ( user_id
         , given_name
         , name
         , email
         , recruitment_channel
         , language
         , terms_accepted_at
         , paused
         , disabled
         , verified ) ->
      let ctx = Pool_tenant.to_ctx db_pool in
      let%lwt user = Service.User.find_by_email_opt ~ctx email in
      match user with
      | None ->
        let%lwt user =
          Service.User.create_user
            ~ctx
            ~id:(user_id |> Pool_common.Id.value)
            ~name
            ~given_name
            ~password
            email
        in
        let%lwt () =
          let address = User.EmailAddress.create email |> get_or_failwith in
          let firstname = User.Firstname.create given_name |> get_or_failwith in
          let lastname = User.Lastname.create name |> get_or_failwith in
          let%lwt () =
            Email.Created (address, user_id, firstname, lastname)
            |> Email.handle_event db_pool
          in
          if CCOption.is_some verified
          then (
            let%lwt _ =
              Service.User.update ~ctx Sihl_user.{ user with confirmed = true }
            in
            let%lwt unverified =
              Email.find_unverified_by_user db_pool user_id
              |> Lwt.map get_or_failwith
            in
            Email.(EmailVerified unverified |> handle_event db_pool))
          else Lwt.return_unit
        in
        Participant.
          { user
          ; recruitment_channel
          ; terms_accepted_at = User.TermsAccepted.create terms_accepted_at
          ; language
          ; paused = User.Paused.create paused
          ; disabled = User.Disabled.create disabled
          ; verified = User.Verified.create verified
          ; firstname_version = Pool_common.Version.create ()
          ; lastname_version = Pool_common.Version.create ()
          ; paused_version = Pool_common.Version.create ()
          ; language_version = Pool_common.Version.create ()
          ; created_at = Ptime_clock.now ()
          ; updated_at = Ptime_clock.now ()
          }
        |> Participant.insert db_pool
      | Some _ ->
        Logs.debug (fun m -> m "%s" "Participant already exists");
        Lwt.return_unit)
    users
;;
