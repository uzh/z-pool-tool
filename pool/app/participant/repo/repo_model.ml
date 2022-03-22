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

module Language = struct
  open Pool_common.Language

  let t =
    let open CCResult in
    Caqti_type.(
      custom
        ~encode:(fun m -> m |> CCOption.map code |> pure)
        ~decode:(fun m ->
          map_err (fun _ ->
              Pool_common.(
                Utils.error_to_string
                  Pool_common.Language.En
                  (Message.Decode Message.Language)))
          @@
          match m with
          | None -> Ok None
          | Some language -> language |> of_string >|= CCOption.some)
        (option string))
  ;;
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
            , ( Paused.value m.paused
              , ( Disabled.value m.disabled
                , ( Verified.value m.verified
                  , ( Version.value m.firstname_version
                    , ( Version.value m.lastname_version
                      , ( Version.value m.paused_version
                        , ( Version.value m.language_version
                          , (m.created_at, m.updated_at) ) ) ) ) ) ) ) ) ) ) )
  in
  let decode
      ( user
      , ( recruitment_channel
        , ( terms_accepted_at
          , ( language
            , ( paused
              , ( disabled
                , ( verified
                  , ( firstname_version
                    , ( lastname_version
                      , ( paused_version
                        , (language_version, (created_at, updated_at)) ) ) ) )
                ) ) ) ) ) )
    =
    let open Pool_user in
    let open CCResult in
    let version_of_int = Pool_common.Version.of_int in
    Ok
      { user
      ; recruitment_channel
      ; terms_accepted_at = TermsAccepted.create terms_accepted_at
      ; language
      ; paused = Paused.create paused
      ; disabled = Disabled.create disabled
      ; verified = Verified.create verified
      ; firstname_version = version_of_int firstname_version
      ; lastname_version = version_of_int lastname_version
      ; paused_version = version_of_int paused_version
      ; language_version = version_of_int language_version
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
      (tup2
         Pool_user.Repo.user_caqti
         (tup2
            RecruitmentChannel.t
            (tup2
               TermsAccepted.t
               (tup2
                  Language.t
                  (tup2
                     Paused.t
                     (tup2
                        Disabled.t
                        (tup2
                           Verified.t
                           (tup2
                              Pool_common.Repo.Version.t
                              (tup2
                                 Pool_common.Repo.Version.t
                                 (tup2
                                    Pool_common.Repo.Version.t
                                    (tup2
                                       Pool_common.Repo.Version.t
                                       (tup2 CreatedAt.t UpdatedAt.t)))))))))))))
;;

let participant =
  let encode m =
    let open Pool_user in
    let open Pool_common in
    Ok
      ( m.user.Sihl_user.id
      , ( m.recruitment_channel
        , ( TermsAccepted.value m.terms_accepted_at
          , ( m.language
            , ( Paused.value m.paused
              , ( Disabled.value m.disabled
                , ( Verified.value m.verified
                  , ( Version.value m.firstname_version
                    , ( Version.value m.lastname_version
                      , ( Version.value m.paused_version
                        , ( Version.value m.language_version
                          , (m.created_at, m.updated_at) ) ) ) ) ) ) ) ) ) ) )
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
                  Language.t
                  (tup2
                     Paused.t
                     (tup2
                        Disabled.t
                        (tup2
                           Verified.t
                           (tup2
                              Pool_common.Repo.Version.t
                              (tup2
                                 Pool_common.Repo.Version.t
                                 (tup2
                                    Pool_common.Repo.Version.t
                                    (tup2
                                       Pool_common.Repo.Version.t
                                       (tup2 CreatedAt.t UpdatedAt.t)))))))))))))
;;

module Write = struct
  open Entity.Write

  let t =
    let encode m =
      let open Pool_user in
      let open Pool_common in
      Ok
        ( Id.value m.user_id
        , ( m.recruitment_channel
          , ( TermsAccepted.value m.terms_accepted_at
            , ( m.language
              , ( Paused.value m.paused
                , ( Disabled.value m.disabled
                  , ( Verified.value m.verified
                    , ( Version.value m.firstname_version
                      , ( Version.value m.lastname_version
                        , ( Version.value m.lastname_version
                          , Version.value m.paused_version ) ) ) ) ) ) ) ) ) )
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
                    Language.t
                    (tup2
                       Paused.t
                       (tup2
                          Disabled.t
                          (tup2
                             Verified.t
                             (tup2
                                Pool_common.Repo.Version.t
                                (tup2
                                   Pool_common.Repo.Version.t
                                   (tup2
                                      Pool_common.Repo.Version.t
                                      Pool_common.Repo.Version.t)))))))))))
  ;;
end
