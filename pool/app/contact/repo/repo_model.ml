open Entity

let make_caqti_type = Pool_common.Repo.make_caqti_type

module NumberOfInvitations = struct
  include NumberOfInvitations

  let t = make_caqti_type Caqti_type.int CCFun.(of_int %> CCResult.return) value
end

module NumberOfAssignments = struct
  include NumberOfAssignments

  let t = make_caqti_type Caqti_type.int CCFun.(of_int %> CCResult.return) value
end

module NumberOfShowUps = struct
  include NumberOfShowUps

  let t = make_caqti_type Caqti_type.int CCFun.(of_int %> CCResult.return) value
end

module NumberOfNoShows = struct
  include NumberOfNoShows

  let t = make_caqti_type Caqti_type.int CCFun.(of_int %> CCResult.return) value
end

module NumberOfParticipations = struct
  include NumberOfParticipations

  let t = make_caqti_type Caqti_type.int CCFun.(of_int %> CCResult.return) value
end

module AdminComment = struct
  include AdminComment

  let t = make_caqti_type Caqti_type.string create value
end

let t =
  let encode m =
    Ok
      ( m.user
      , ( m.terms_accepted_at
        , ( m.language
          , ( m.experiment_type_preference
            , ( m.cell_phone
              , ( m.paused
                , ( m.disabled
                  , ( m.verified
                    , ( m.email_verified
                      , ( m.num_invitations
                        , ( m.num_assignments
                          , ( m.num_show_ups
                            , ( m.num_no_shows
                              , ( m.num_participations
                                , ( m.firstname_version
                                  , ( m.lastname_version
                                    , ( m.paused_version
                                      , ( m.language_version
                                        , ( m.experiment_type_preference_version
                                          , ( m.import_pending
                                            , (m.created_at, m.updated_at) ) )
                                        ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) )
  in
  let decode
    ( user
    , ( terms_accepted_at
      , ( language
        , ( experiment_type_preference
          , ( cell_phone
            , ( paused
              , ( disabled
                , ( verified
                  , ( email_verified
                    , ( num_invitations
                      , ( num_assignments
                        , ( num_show_ups
                          , ( num_no_shows
                            , ( num_participations
                              , ( firstname_version
                                , ( lastname_version
                                  , ( paused_version
                                    , ( language_version
                                      , ( experiment_type_preference_version
                                        , ( import_pending
                                          , (created_at, updated_at) ) ) ) ) )
                                ) ) ) ) ) ) ) ) ) ) ) ) ) ) )
    =
    let open CCResult in
    Ok
      { user
      ; terms_accepted_at
      ; language
      ; experiment_type_preference
      ; cell_phone
      ; paused
      ; disabled
      ; verified
      ; email_verified
      ; num_invitations = NumberOfInvitations.of_int num_invitations
      ; num_assignments = NumberOfAssignments.of_int num_assignments
      ; num_show_ups = NumberOfShowUps.of_int num_show_ups
      ; num_no_shows = NumberOfNoShows.of_int num_no_shows
      ; num_participations = NumberOfParticipations.of_int num_participations
      ; firstname_version
      ; lastname_version
      ; paused_version
      ; language_version
      ; experiment_type_preference_version
      ; import_pending
      ; created_at
      ; updated_at
      }
  in
  let open Pool_user.Repo in
  let open Pool_common.Repo in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (t2
         t
         (t2
            (option TermsAccepted.t)
            (t2
               (option Language.t)
               (t2
                  (option Pool_common.Repo.ExperimentType.t)
                  (t2
                     (option CellPhone.t)
                     (t2
                        Paused.t
                        (t2
                           Disabled.t
                           (t2
                              (option Verified.t)
                              (t2
                                 (option EmailVerified.t)
                                 (t2
                                    NumberOfInvitations.t
                                    (t2
                                       NumberOfAssignments.t
                                       (t2
                                          NumberOfShowUps.t
                                          (t2
                                             NumberOfNoShows.t
                                             (t2
                                                NumberOfParticipations.t
                                                (t2
                                                   Pool_common.Repo.Version.t
                                                   (t2
                                                      Pool_common.Repo.Version.t
                                                      (t2
                                                         Pool_common.Repo
                                                         .Version
                                                         .t
                                                         (t2
                                                            Pool_common.Repo
                                                            .Version
                                                            .t
                                                            (t2
                                                               Pool_common.Repo
                                                               .Version
                                                               .t
                                                               (t2
                                                                  Pool_user.Repo
                                                                  .ImportPending
                                                                  .t
                                                                  (t2
                                                                     CreatedAt.t
                                                                     UpdatedAt.t))))))))))))))))))))))
;;

let contact =
  let encode m =
    Ok
      ( m.user.Pool_user.id
      , ( m.terms_accepted_at
        , ( m.language
          , ( m.experiment_type_preference
            , ( m.paused
              , ( m.disabled
                , ( m.verified
                  , ( m.email_verified
                    , ( m.num_invitations
                      , ( m.num_assignments
                        , ( m.num_show_ups
                          , ( m.num_no_shows
                            , ( m.num_participations
                              , ( m.firstname_version
                                , ( m.lastname_version
                                  , ( m.paused_version
                                    , ( m.language_version
                                      , ( m.experiment_type_preference_version
                                        , ( m.import_pending
                                          , (m.created_at, m.updated_at) ) ) )
                                    ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) )
  in
  let decode _ =
    Pool_message.Error.WriteOnlyModel |> Pool_common.Utils.failwith
  in
  let open Pool_common.Repo in
  let open Pool_user.Repo in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (t2
         Id.t
         (t2
            (option TermsAccepted.t)
            (t2
               (option Language.t)
               (t2
                  (option Pool_common.Repo.ExperimentType.t)
                  (t2
                     Paused.t
                     (t2
                        Disabled.t
                        (t2
                           (option Verified.t)
                           (t2
                              (option EmailVerified.t)
                              (t2
                                 NumberOfInvitations.t
                                 (t2
                                    NumberOfAssignments.t
                                    (t2
                                       NumberOfAssignments.t
                                       (t2
                                          NumberOfShowUps.t
                                          (t2
                                             NumberOfNoShows.t
                                             (t2
                                                Pool_common.Repo.Version.t
                                                (t2
                                                   Pool_common.Repo.Version.t
                                                   (t2
                                                      Pool_common.Repo.Version.t
                                                      (t2
                                                         Pool_common.Repo
                                                         .Version
                                                         .t
                                                         (t2
                                                            Pool_common.Repo
                                                            .Version
                                                            .t
                                                            (t2
                                                               Pool_user.Repo
                                                               .ImportPending
                                                               .t
                                                               (t2
                                                                  CreatedAt.t
                                                                  UpdatedAt.t)))))))))))))))))))))
;;

module Write = struct
  open Entity.Write

  let t =
    let encode m =
      Ok
        ( m.user_id
        , ( m.terms_accepted_at
          , ( m.language
            , ( m.experiment_type_preference
              , ( m.cell_phone
                , ( m.paused
                  , ( m.disabled
                    , ( m.verified
                      , ( m.email_verified
                        , ( m.num_invitations
                          , ( m.num_assignments
                            , ( m.num_show_ups
                              , ( m.num_no_shows
                                , ( m.num_participations
                                  , ( m.firstname_version
                                    , ( m.lastname_version
                                      , ( m.lastname_version
                                        , ( m.paused_version
                                          , ( m
                                                .experiment_type_preference_version
                                            , m.import_pending ) ) ) ) ) ) ) )
                            ) ) ) ) ) ) ) ) ) ) )
    in
    let decode _ =
      Pool_message.Error.WriteOnlyModel |> Pool_common.Utils.failwith
    in
    let open Pool_common.Repo in
    let open Pool_user.Repo in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (t2
           Id.t
           (t2
              (option TermsAccepted.t)
              (t2
                 (option Language.t)
                 (t2
                    (option Pool_common.Repo.ExperimentType.t)
                    (t2
                       (option CellPhone.t)
                       (t2
                          Paused.t
                          (t2
                             Disabled.t
                             (t2
                                (option Verified.t)
                                (t2
                                   (option EmailVerified.t)
                                   (t2
                                      NumberOfInvitations.t
                                      (t2
                                         NumberOfAssignments.t
                                         (t2
                                            NumberOfShowUps.t
                                            (t2
                                               NumberOfNoShows.t
                                               (t2
                                                  NumberOfParticipations.t
                                                  (t2
                                                     Pool_common.Repo.Version.t
                                                     (t2
                                                        Pool_common.Repo.Version
                                                        .t
                                                        (t2
                                                           Pool_common.Repo
                                                           .Version
                                                           .t
                                                           (t2
                                                              Pool_common.Repo
                                                              .Version
                                                              .t
                                                              (t2
                                                                 Pool_common
                                                                 .Repo
                                                                 .Version
                                                                 .t
                                                                 Pool_user.Repo
                                                                 .ImportPending
                                                                 .t))))))))))))))))))))
  ;;
end

module Preview = struct
  open Entity.Preview

  let t =
    let encode (m : t) =
      Ok
        ( m.user
        , ( m.language
          , ( m.cell_phone
            , (m.paused, (m.verified, (m.num_invitations, m.num_assignments)))
            ) ) )
    in
    let decode
      ( user
      , ( language
        , (cell_phone, (paused, (verified, (num_invitations, num_assignments))))
        ) )
      =
      let open CCResult in
      Ok
        Entity.Preview.
          { user
          ; language
          ; cell_phone
          ; paused
          ; verified
          ; num_invitations = NumberOfInvitations.of_int num_invitations
          ; num_assignments = NumberOfAssignments.of_int num_assignments
          }
    in
    let open Pool_user.Repo in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (t2
           t
           (t2
              (option Pool_common.Repo.Language.t)
              (t2
                 (option CellPhone.t)
                 (t2
                    Paused.t
                    (t2
                       (option Verified.t)
                       (t2 NumberOfInvitations.t NumberOfAssignments.t)))))))
  ;;
end
