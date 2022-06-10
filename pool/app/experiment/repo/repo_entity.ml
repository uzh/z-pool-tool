open Entity
module Common = Pool_common
module Id = Common.Id
module RepoId = Common.Repo.Id

module Title = struct
  include Title

  let t =
    let encode = Utils.fcn_ok value in
    let decode m =
      m |> create |> CCResult.map_err Common.(Utils.error_to_string Language.En)
    in
    Caqti_type.(custom ~encode ~decode string)
  ;;
end

module Description = struct
  include Description

  let t =
    let encode = Utils.fcn_ok value in
    let decode m =
      m |> create |> CCResult.map_err Common.(Utils.error_to_string Language.En)
    in
    Caqti_type.(custom ~encode ~decode string)
  ;;
end

module WaitingListDisabled = struct
  include WaitingListDisabled

  let t = Caqti_type.bool
end

module DirectRegistrationDisabled = struct
  include DirectRegistrationDisabled

  let t = Caqti_type.bool
end

module RegistrationDisabled = struct
  include RegistrationDisabled

  let t = Caqti_type.bool
end

let t =
  let encode (m : t) =
    Ok
      ( m.id
      , ( Title.value m.title
        , ( m.description
          , ( m.filter
            , ( m.waiting_list_disabled
              , ( m.direct_registration_disabled
                , (m.registration_disabled, (m.created_at, m.updated_at)) ) ) )
          ) ) )
  in
  let decode
      ( id
      , ( title
        , ( description
          , ( filter
            , ( waiting_list_disabled
              , ( direct_registration_disabled
                , (registration_disabled, (created_at, updated_at)) ) ) ) ) ) )
    =
    let open CCResult in
    Ok
      { id
      ; title
      ; description
      ; filter
      ; mailings = []
      ; waiting_list_disabled
      ; direct_registration_disabled
      ; registration_disabled
      ; created_at
      ; updated_at
      }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         RepoId.t
         (tup2
            Title.t
            (tup2
               Description.t
               (tup2
                  string
                  (tup2
                     WaitingListDisabled.t
                     (tup2
                        DirectRegistrationDisabled.t
                        (tup2
                           RegistrationDisabled.t
                           (tup2
                              Common.Repo.CreatedAt.t
                              Common.Repo.UpdatedAt.t)))))))))
;;

module Write = struct
  let t =
    let encode (m : t) =
      Ok
        ( m.id
        , ( Title.value m.title
          , ( Description.value m.description
            , ( m.filter
              , ( m.waiting_list_disabled
                , (m.direct_registration_disabled, m.registration_disabled) ) )
            ) ) )
    in
    let decode _ = failwith "Write only model" in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2
           RepoId.t
           (tup2
              Title.t
              (tup2
                 Description.t
                 (tup2
                    string
                    (tup2
                       WaitingListDisabled.t
                       (tup2
                          DirectRegistrationDisabled.t
                          RegistrationDisabled.t)))))))
  ;;
end

module Public = struct
  open Entity.Public

  let t =
    let encode (m : t) =
      Ok
        ( m.id
        , ( m.description
          , (m.waiting_list_disabled, m.direct_registration_disabled) ) )
    in
    let decode
        ( id
        , (description, (waiting_list_disabled, direct_registration_disabled))
        )
      =
      Ok
        { id; description; waiting_list_disabled; direct_registration_disabled }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2
           RepoId.t
           (tup2
              Description.t
              (tup2 WaitingListDisabled.t DirectRegistrationDisabled.t))))
  ;;
end
