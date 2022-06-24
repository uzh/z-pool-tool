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

module PublicTitle = struct
  include PublicTitle

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
        , ( PublicTitle.value m.public_title
          , ( m.description
            , ( m.filter
              , ( m.direct_registration_disabled
                , ( m.registration_disabled
                  , ( m.session_reminder_lead_time
                    , ( m.session_reminder_subject
                      , (m.session_reminder_text, (m.created_at, m.updated_at))
                      ) ) ) ) ) ) ) ) )
  in
  let decode
      ( id
      , ( title
        , ( public_title
          , ( description
            , ( filter
              , ( direct_registration_disabled
                , ( registration_disabled
                  , ( session_reminder_lead_time
                    , ( session_reminder_subject
                      , (session_reminder_text, (created_at, updated_at)) ) ) )
                ) ) ) ) ) )
    =
    let open CCResult in
    Ok
      { id
      ; title
      ; public_title
      ; description
      ; filter
      ; direct_registration_disabled
      ; registration_disabled
      ; session_reminder_lead_time
      ; session_reminder_subject
      ; session_reminder_text
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
               PublicTitle.t
               (tup2
                  Description.t
                  (tup2
                     string
                     (tup2
                        DirectRegistrationDisabled.t
                        (tup2
                           RegistrationDisabled.t
                           (tup2
                              (option Pool_common.Repo.Reminder.LeadTime.t)
                              (tup2
                                 (option Pool_common.Repo.Reminder.Subject.t)
                                 (tup2
                                    (option Pool_common.Repo.Reminder.Text.t)
                                    (tup2
                                       Common.Repo.CreatedAt.t
                                       Common.Repo.UpdatedAt.t))))))))))))
;;

module Write = struct
  let t =
    let encode (m : t) =
      Ok
        ( m.id
        , ( Title.value m.title
          , ( PublicTitle.value m.public_title
            , ( Description.value m.description
              , ( m.filter
                , ( m.direct_registration_disabled
                  , ( m.registration_disabled
                    , ( m.session_reminder_lead_time
                      , (m.session_reminder_subject, m.session_reminder_text) )
                    ) ) ) ) ) ) )
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
                 PublicTitle.t
                 (tup2
                    Description.t
                    (tup2
                       string
                       (tup2
                          DirectRegistrationDisabled.t
                          (tup2
                             RegistrationDisabled.t
                             (tup2
                                (option Pool_common.Repo.Reminder.LeadTime.t)
                                (tup2
                                   (option Pool_common.Repo.Reminder.Subject.t)
                                   (option Pool_common.Repo.Reminder.Text.t)))))))))))
  ;;
end

module Public = struct
  open Entity.Public

  let t =
    let encode (m : t) =
      Ok
        (m.id, (m.public_title, (m.description, m.direct_registration_disabled)))
    in
    let decode (id, (public_title, (description, direct_registration_disabled)))
      =
      Ok { id; public_title; description; direct_registration_disabled }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2
           RepoId.t
           (tup2
              PublicTitle.t
              (tup2 Description.t DirectRegistrationDisabled.t))))
  ;;
end
