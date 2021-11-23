open Entity

module RecruitmentChannel = struct
  include RecruitmentChannel

  let t =
    let open CCResult in
    Caqti_type.(
      custom
        ~encode:(fun m -> m |> to_string |> pure)
        ~decode:(fun m ->
          map_err (fun _ -> "decode recruitment channel") @@ of_string m)
        string)
  ;;
end

let t =
  let encode m =
    let open Common_user in
    let open Pool_common.ChangeSet in
    Ok
      ( m.user
      , ( m.recruitment_channel
        , ( TermsAccepted.value m.terms_accepted_at
          , ( Paused.value m.paused
            , ( Disabled.value m.disabled
              , ( Verified.value m.verified
                , ( Version.value m.firstname_version
                  , ( Version.value m.lastname_version
                    , ( Version.value m.paused_version
                      , (m.created_at, m.updated_at) ) ) ) ) ) ) ) ) )
  in
  let decode
      ( user
      , ( recruitment_channel
        , ( terms_accepted_at
          , ( paused
            , ( disabled
              , ( verified
                , ( firstname_version
                  , ( lastname_version
                    , (paused_version, (created_at, updated_at)) ) ) ) ) ) ) )
      )
    =
    let open Common_user in
    Ok
      { user
      ; recruitment_channel
      ; terms_accepted_at = TermsAccepted.create terms_accepted_at
      ; paused = Paused.create paused
      ; disabled = Disabled.create disabled
      ; verified = Verified.create verified
      ; firstname_version =
          Pool_common.ChangeSet.Version.of_int firstname_version
      ; lastname_version = Pool_common.ChangeSet.Version.of_int lastname_version
      ; paused_version = Pool_common.ChangeSet.Version.of_int paused_version
      ; created_at
      ; updated_at
      }
  in
  let open Common_user.Repo in
  let open Pool_common.Repo in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         Common_user.Repo.user_caqti
         (tup2
            RecruitmentChannel.t
            (tup2
               TermsAccepted.t
               (tup2
                  Paused.t
                  (tup2
                     Disabled.t
                     (tup2
                        Verified.t
                        (tup2
                           Pool_common.Repo.ChangeSet.Version.t
                           (tup2
                              Pool_common.Repo.ChangeSet.Version.t
                              (tup2
                                 Pool_common.Repo.ChangeSet.Version.t
                                 (tup2 CreatedAt.t UpdatedAt.t)))))))))))
;;

let participant =
  let encode m =
    let open Common_user in
    let open Pool_common.ChangeSet in
    Ok
      ( m.user.Sihl_user.id
      , ( m.recruitment_channel
        , ( TermsAccepted.value m.terms_accepted_at
          , ( Paused.value m.paused
            , ( Disabled.value m.disabled
              , ( Verified.value m.verified
                , ( Version.value m.firstname_version
                  , ( Version.value m.lastname_version
                    , ( Version.value m.paused_version
                      , (m.created_at, m.updated_at) ) ) ) ) ) ) ) ) )
  in
  let decode _ =
    failwith
      Pool_common.(Message.WriteOnlyModel |> Utils.error_to_string Language.En)
  in
  let open Common_user.Repo in
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
                  Paused.t
                  (tup2
                     Disabled.t
                     (tup2
                        Verified.t
                        (tup2
                           Pool_common.Repo.ChangeSet.Version.t
                           (tup2
                              Pool_common.Repo.ChangeSet.Version.t
                              (tup2
                                 Pool_common.Repo.ChangeSet.Version.t
                                 (tup2 CreatedAt.t UpdatedAt.t)))))))))))
;;
