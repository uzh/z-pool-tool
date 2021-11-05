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
    Ok
      ( m.user
      , ( m.recruitment_channel
        , ( TermsAccepted.value m.terms_accepted_at
          , ( Paused.value m.paused
            , ( Disabled.value m.disabled
              , (Verified.value m.verified, (m.created_at, m.updated_at)) ) ) )
        ) )
  in
  let decode
      ( user
      , ( recruitment_channel
        , ( terms_accepted_at
          , (paused, (disabled, (verified, (created_at, updated_at)))) ) ) )
    =
    let open Common_user in
    Ok
      { user
      ; recruitment_channel
      ; terms_accepted_at = TermsAccepted.create terms_accepted_at
      ; paused = Paused.create paused
      ; disabled = Disabled.create disabled
      ; verified = Verified.create verified
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
                     (tup2 Verified.t (tup2 CreatedAt.t UpdatedAt.t))))))))
;;

let participant =
  let encode m =
    let open Common_user in
    Ok
      ( m.user.Sihl_user.id
      , ( m.recruitment_channel
        , ( TermsAccepted.value m.terms_accepted_at
          , ( Paused.value m.paused
            , ( Disabled.value m.disabled
              , (Verified.value m.verified, (m.created_at, m.updated_at)) ) ) )
        ) )
  in
  let decode _ = Error "Model only used for DB insert" in
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
                     (tup2 Verified.t (tup2 CreatedAt.t UpdatedAt.t))))))))
;;
