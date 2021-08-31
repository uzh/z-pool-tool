open Entity

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
    (* TODO checks for confirmed users only, a Person should just be valid, if
       it was confirmed. Check if there is a better place for this. *)
    if not confirmed
    then Error "User is not confirmed"
    else
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

let encode_person
    : type a.
      [< `Assistant
      | `Experimenter
      | `LocationManager
      | `Operator
      | `Participant
      | `Recruiter
      ]
      -> a Entity.t
      -> ( string
           * (Sihl_user.t
             * (RecruitmentChannel.t
               * (TermsAccepted.t
                 * (Paused.t
                   * (Disabled.t * (Verified.t * (Ptime.t * Ptime.t)))))))
         , 'b )
         result
  =
 fun carrier person ->
  match person with
  | Participant m
  | Assistant m
  | Experimenter m
  | LocationManager m
  | Recruiter m
  | Operator m ->
    let role = carrier |> Utils.Stringify.person in
    Ok
      ( role
      , ( m.user
        , ( m.recruitment_channel
          , ( m.terms_accepted_at
            , ( m.paused
              , (m.disabled, (m.verified, (m.created_at, m.updated_at))) ) ) )
        ) )
;;

let decode_person
    constructor
    ( _
    , ( user
      , ( recruitment_channel
        , ( terms_accepted_at
          , (paused, (disabled, (verified, (created_at, updated_at)))) ) ) ) )
  =
  Ok
    (constructor
       { user
       ; recruitment_channel
       ; terms_accepted_at
       ; paused
       ; disabled
       ; verified
       ; created_at
       ; updated_at
       })
;;

let make_caqti_type encode decode =
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         string
         (tup2
            user_caqti
            (tup2
               RecruitmentChannel.t
               (tup2
                  TermsAccepted.t
                  (tup2
                     Paused.t
                     (tup2 Disabled.t (tup2 Verified.t (tup2 ptime ptime)))))))))
;;

let participant =
  make_caqti_type (encode_person `Participant)
  @@ decode_person (fun person -> Participant person)
;;

let assistant =
  make_caqti_type (encode_person `Assistant)
  @@ decode_person (fun person -> Assistant person)
;;

let experimenter =
  make_caqti_type (encode_person `Experimenter)
  @@ decode_person (fun person -> Experimenter person)
;;

let location_manager =
  make_caqti_type (encode_person `LocationManager)
  @@ decode_person (fun person -> LocationManager person)
;;

let recruiter =
  make_caqti_type (encode_person `Recruiter)
  @@ decode_person (fun person -> Recruiter person)
;;

let operator =
  make_caqti_type (encode_person `Operator)
  @@ decode_person (fun person -> Operator person)
;;
