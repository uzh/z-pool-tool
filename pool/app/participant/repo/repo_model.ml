open Entity

module RecruitmentChannel = struct
  include RecruitmentChannel

  let t =
    Caqti_type.(
      custom
        ~encode:(fun m -> m |> to_string |> Result.ok)
        ~decode:of_string
        string)
  ;;
end

let user_caqti =
  let status =
    let encode m = m |> Sihl_user.status_to_string |> Result.ok in
    let decode = Sihl_user.status_of_string in
    Caqti_type.(custom ~encode ~decode string)
  in
  let open Sihl.Contract.User in
  let encode m =
    Ok
      ( m.id
      , ( m.email
        , ( m.username
          , ( m.name
            , ( m.given_name
              , ( m.password
                , ( m.status
                  , (m.admin, (m.confirmed, (m.created_at, m.updated_at))) ) )
              ) ) ) ) )
  in
  let decode
      ( id
      , ( email
        , ( username
          , ( name
            , ( given_name
              , ( password
                , (status, (admin, (confirmed, (created_at, updated_at)))) ) )
            ) ) ) )
    =
    (* TODO checks for confirmed and not admin users only, a Person should just
       be valid, if it was confirmed. Check if there is a better place for
       this. *)
    match confirmed, admin with
    | false, _ -> Error "User is not confirmed"
    | _, true -> Error "An admin cannot be a participant"
    | _ ->
      Ok
        { id
        ; email
        ; username
        ; name
        ; given_name
        ; password
        ; status
        ; admin
        ; confirmed
        ; created_at
        ; updated_at
        }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         string
         (tup2
            string
            (tup2
               (option string)
               (tup2
                  (option string)
                  (tup2
                     (option string)
                     (tup2
                        string
                        (tup2 status (tup2 bool (tup2 bool (tup2 ptime ptime)))))))))))
;;

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
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         user_caqti
         (tup2
            RecruitmentChannel.t
            (tup2
               TermsAccepted.t
               (tup2
                  Paused.t
                  (tup2 Disabled.t (tup2 Verified.t (tup2 ptime ptime))))))))
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
                  (tup2 Disabled.t (tup2 Verified.t (tup2 ptime ptime))))))))
;;
