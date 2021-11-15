let admins db_pool () =
  let data =
    [ "The", "One", "admin@example.com", `Operator
    ; "engineering", "admin", "engineering@econ.uzh.ch", `Operator
    ]
  in
  let ctx = Pool_common.Utils.pool_to_ctx db_pool in
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
        let%lwt _ =
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
    [ ( "Hansruedi"
      , "Rüdisüli"
      , "one@test.com"
      , Participant.RecruitmentChannel.Friend
      , Some (Ptime_clock.now ())
      , false
      , false
      , Some (Ptime_clock.now ()) )
    ; ( "Jane"
      , "Doe"
      , "two@test.com"
      , Participant.RecruitmentChannel.Online
      , Some (Ptime_clock.now ())
      , false
      , false
      , None )
    ; ( "John"
      , "Dorrian"
      , "three@mail.com"
      , Participant.RecruitmentChannel.Lecture
      , Some (Ptime_clock.now ())
      , true
      , false
      , Some (Ptime_clock.now ()) )
    ; ( "Kevin"
      , "McCallistor"
      , "four@mail.com"
      , Participant.RecruitmentChannel.Mailing
      , Some (Ptime_clock.now ())
      , true
      , false
      , None )
    ; ( "Hello"
      , "Kitty"
      , "five@mail.com"
      , Participant.RecruitmentChannel.Online
      , Some (Ptime_clock.now ())
      , true
      , true
      , Some (Ptime_clock.now ()) )
    ; ( "Dr."
      , "Murphy"
      , "six@mail.com"
      , Participant.RecruitmentChannel.Friend
      , Some (Ptime_clock.now ())
      , true
      , true
      , None )
    ; ( "Mr."
      , "Do not accept terms"
      , "six@mail.com"
      , Participant.RecruitmentChannel.Friend
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
    (fun ( given_name
         , name
         , email
         , recruitment_channel
         , terms_accepted_at
         , paused
         , disabled
         , verified ) ->
      let ctx = Pool_common.Utils.pool_to_ctx db_pool in
      let%lwt user = Service.User.find_by_email_opt ~ctx email in
      match user with
      | None ->
        let%lwt user =
          Service.User.create_user ~ctx ~name ~given_name ~password email
        in
        Participant.
          { user
          ; recruitment_channel
          ; terms_accepted_at =
              Common_user.TermsAccepted.create terms_accepted_at
          ; paused = Common_user.Paused.create paused
          ; disabled = Common_user.Disabled.create disabled
          ; verified = Common_user.Verified.create verified
          ; created_at = Ptime_clock.now ()
          ; updated_at = Ptime_clock.now ()
          }
        |> Participant.insert db_pool
      | Some _ ->
        Logs.debug (fun m -> m "%s" "Participant already exists");
        Lwt.return_unit)
    users
;;
