open Entity

module RecruitmentChannel = struct
  include RecruitmentChannel

  let t =
    let open CCResult in
    Caqti_type.(
      custom
        ~encode:(fun m -> m |> yojson_of_t |> Yojson.Safe.to_string |> pure)
        ~decode:(fun m -> m |> Yojson.Safe.from_string |> t_of_yojson |> pure)
        string)
  ;;
end

module NumberOfInvitations = struct
  include NumberOfInvitations

  let t = Caqti_type.int
end

module NumberOfAssignments = struct
  include NumberOfAssignments

  let t = Caqti_type.int
end

let t =
  let encode m =
    let open Pool_user in
    let open Pool_common in
    Ok
      ( m.user
      , ( m.recruitment_channel
        , ( TermsAccepted.value m.terms_accepted_at
          , ( m.language
            , ( m.experiment_type_preference
              , ( Paused.value m.paused
                , ( Disabled.value m.disabled
                  , ( Verified.value m.verified
                    , ( EmailVerified.value m.email_verified
                      , ( NumberOfInvitations.value m.num_invitations
                        , ( NumberOfAssignments.value m.num_assignments
                          , ( Version.value m.firstname_version
                            , ( Version.value m.lastname_version
                              , ( Version.value m.paused_version
                                , ( Version.value m.language_version
                                  , ( Version.value
                                        m.experiment_type_preference_version
                                    , (m.created_at, m.updated_at) ) ) ) ) ) )
                        ) ) ) ) ) ) ) ) ) )
  in
  let decode
    ( user
    , ( recruitment_channel
      , ( terms_accepted_at
        , ( language
          , ( experiment_type_preference
            , ( paused
              , ( disabled
                , ( verified
                  , ( email_verified
                    , ( num_invitations
                      , ( num_assignments
                        , ( firstname_version
                          , ( lastname_version
                            , ( paused_version
                              , ( language_version
                                , ( experiment_type_preference_version
                                  , (created_at, updated_at) ) ) ) ) ) ) ) ) )
                ) ) ) ) ) ) )
    =
    let open Pool_user in
    let open CCResult in
    Pool_common.Version.(
      Ok
        { user
        ; recruitment_channel
        ; terms_accepted_at = TermsAccepted.create terms_accepted_at
        ; language
        ; experiment_type_preference
        ; paused = Paused.create paused
        ; disabled = Disabled.create disabled
        ; verified = Verified.create verified
        ; email_verified = EmailVerified.create email_verified
        ; num_invitations = NumberOfInvitations.of_int num_invitations
        ; num_assignments = NumberOfAssignments.of_int num_assignments
        ; firstname_version = of_int firstname_version
        ; lastname_version = of_int lastname_version
        ; paused_version = of_int paused_version
        ; language_version = of_int language_version
        ; experiment_type_preference_version =
            of_int experiment_type_preference_version
        ; created_at
        ; updated_at
        })
  in
  let open Pool_user.Repo in
  let open Pool_common.Repo in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         Pool_user.Repo.user_caqti
         (tup2
            RecruitmentChannel.t
            (tup2
               TermsAccepted.t
               (tup2
                  (option Language.t)
                  (tup2
                     (option Pool_common.Repo.ExperimentType.t)
                     (tup2
                        Paused.t
                        (tup2
                           Disabled.t
                           (tup2
                              Verified.t
                              (tup2
                                 EmailVerified.t
                                 (tup2
                                    NumberOfInvitations.t
                                    (tup2
                                       NumberOfAssignments.t
                                       (tup2
                                          Pool_common.Repo.Version.t
                                          (tup2
                                             Pool_common.Repo.Version.t
                                             (tup2
                                                Pool_common.Repo.Version.t
                                                (tup2
                                                   Pool_common.Repo.Version.t
                                                   (tup2
                                                      Pool_common.Repo.Version.t
                                                      (tup2
                                                         CreatedAt.t
                                                         UpdatedAt.t))))))))))))))))))
;;

let contact =
  let encode m =
    let open Pool_user in
    let open Pool_common in
    Ok
      ( m.user.Sihl_user.id
      , ( m.recruitment_channel
        , ( TermsAccepted.value m.terms_accepted_at
          , ( m.language
            , ( m.experiment_type_preference
              , ( Paused.value m.paused
                , ( Disabled.value m.disabled
                  , ( Verified.value m.verified
                    , ( EmailVerified.value m.email_verified
                      , ( NumberOfInvitations.value m.num_invitations
                        , ( NumberOfAssignments.value m.num_assignments
                          , ( Version.value m.firstname_version
                            , ( Version.value m.lastname_version
                              , ( Version.value m.paused_version
                                , ( Version.value m.language_version
                                  , ( Version.value
                                        m.experiment_type_preference_version
                                    , (m.created_at, m.updated_at) ) ) ) ) ) )
                        ) ) ) ) ) ) ) ) ) )
  in
  let decode _ =
    failwith
      Pool_common.(Message.WriteOnlyModel |> Utils.error_to_string Language.En)
  in
  let open Pool_user.Repo in
  let open Pool_common.Repo in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         string
         (tup2
            RecruitmentChannel.t
            (tup2
               TermsAccepted.t
               (tup2
                  (option Language.t)
                  (tup2
                     (option Pool_common.Repo.ExperimentType.t)
                     (tup2
                        Paused.t
                        (tup2
                           Disabled.t
                           (tup2
                              Verified.t
                              (tup2
                                 EmailVerified.t
                                 (tup2
                                    NumberOfInvitations.t
                                    (tup2
                                       NumberOfAssignments.t
                                       (tup2
                                          Pool_common.Repo.Version.t
                                          (tup2
                                             Pool_common.Repo.Version.t
                                             (tup2
                                                Pool_common.Repo.Version.t
                                                (tup2
                                                   Pool_common.Repo.Version.t
                                                   (tup2
                                                      Pool_common.Repo.Version.t
                                                      (tup2
                                                         CreatedAt.t
                                                         UpdatedAt.t))))))))))))))))))
;;

module Write = struct
  open Entity.Write

  let t =
    let encode m =
      let open Pool_user in
      let open Pool_common in
      Ok
        ( m.user_id
        , ( m.recruitment_channel
          , ( TermsAccepted.value m.terms_accepted_at
            , ( m.language
              , ( m.experiment_type_preference
                , ( Paused.value m.paused
                  , ( Disabled.value m.disabled
                    , ( Verified.value m.verified
                      , ( EmailVerified.value m.email_verified
                        , ( NumberOfInvitations.value m.num_invitations
                          , ( NumberOfAssignments.value m.num_assignments
                            , ( Version.value m.firstname_version
                              , ( Version.value m.lastname_version
                                , ( Version.value m.lastname_version
                                  , ( Version.value m.paused_version
                                    , Version.value
                                        m.experiment_type_preference_version )
                                  ) ) ) ) ) ) ) ) ) ) ) ) ) )
    in
    let decode _ =
      failwith
        Pool_common.(
          Message.WriteOnlyModel |> Utils.error_to_string Language.En)
    in
    let open Pool_user.Repo in
    let open Pool_common.Repo in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2
           Id.t
           (tup2
              RecruitmentChannel.t
              (tup2
                 TermsAccepted.t
                 (tup2
                    (option Language.t)
                    (tup2
                       (option Pool_common.Repo.ExperimentType.t)
                       (tup2
                          Paused.t
                          (tup2
                             Disabled.t
                             (tup2
                                Verified.t
                                (tup2
                                   EmailVerified.t
                                   (tup2
                                      NumberOfInvitations.t
                                      (tup2
                                         NumberOfAssignments.t
                                         (tup2
                                            Pool_common.Repo.Version.t
                                            (tup2
                                               Pool_common.Repo.Version.t
                                               (tup2
                                                  Pool_common.Repo.Version.t
                                                  (tup2
                                                     Pool_common.Repo.Version.t
                                                     Pool_common.Repo.Version.t))))))))))))))))
  ;;
end

module Preview = struct
  open Entity.Preview

  let t =
    let encode (m : t) =
      let open Pool_user in
      Ok
        ( m.user
        , ( m.language
          , ( Paused.value m.paused
            , ( Verified.value m.verified
              , ( NumberOfInvitations.value m.num_invitations
                , NumberOfAssignments.value m.num_assignments ) ) ) ) )
    in
    let decode
      ( user
      , (language, (paused, (verified, (num_invitations, num_assignments)))) )
      =
      let open Pool_user in
      let open CCResult in
      Ok
        Entity.Preview.
          { user
          ; language
          ; paused = Paused.create paused
          ; verified = Verified.create verified
          ; num_invitations = NumberOfInvitations.of_int num_invitations
          ; num_assignments = NumberOfAssignments.of_int num_assignments
          }
    in
    let open Pool_user.Repo in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2
           Pool_user.Repo.user_caqti
           (tup2
              (option Pool_common.Repo.Language.t)
              (tup2
                 Paused.t
                 (tup2
                    Verified.t
                    (tup2 NumberOfInvitations.t NumberOfAssignments.t))))))
  ;;
end
