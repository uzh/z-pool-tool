open CCFun
open Entity
module Common = Pool_common
module Id = Common.Id
module RepoId = Common.Repo.Id

module Title = struct
  include Title

  let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
end

module PublicTitle = struct
  include PublicTitle

  let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
end

module Description = struct
  include Description

  let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
end

module CostCenter = struct
  include CostCenter

  let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
end

module DirectRegistrationDisabled = struct
  include DirectRegistrationDisabled

  let t =
    Pool_common.Repo.make_caqti_type
      Caqti_type.bool
      (create %> CCResult.return)
      value
  ;;
end

module RegistrationDisabled = struct
  include RegistrationDisabled

  let t =
    Pool_common.Repo.make_caqti_type
      Caqti_type.bool
      (create %> CCResult.return)
      value
  ;;
end

module AllowUninvitedSignup = struct
  include AllowUninvitedSignup

  let t =
    Pool_common.Repo.make_caqti_type
      Caqti_type.bool
      (create %> CCResult.return)
      value
  ;;
end

let t =
  let encode (m : t) =
    Ok
      ( m.id
      , ( m.title
        , ( m.public_title
          , ( m.description
            , ( m.cost_center
              , ( m.organisational_unit
                , ( m.filter
                  , ( m.contact_person_id
                    , ( m.direct_registration_disabled
                      , ( m.registration_disabled
                        , ( m.allow_uninvited_signup
                          , ( m.experiment_type
                            , ( m.session_reminder_lead_time
                              , (m.created_at, m.updated_at) ) ) ) ) ) ) ) ) )
            ) ) ) )
  in
  let decode
    ( id
    , ( title
      , ( public_title
        , ( description
          , ( cost_center
            , ( organisational_unit
              , ( filter
                , ( contact_person_id
                  , ( direct_registration_disabled
                    , ( registration_disabled
                      , ( allow_uninvited_signup
                        , ( experiment_type
                          , ( session_reminder_lead_time
                            , (created_at, updated_at) ) ) ) ) ) ) ) ) ) ) ) )
    )
    =
    let open CCResult in
    Ok
      { id
      ; title
      ; public_title
      ; description
      ; cost_center
      ; contact_person_id
      ; organisational_unit
      ; filter
      ; direct_registration_disabled
      ; registration_disabled
      ; allow_uninvited_signup
      ; experiment_type
      ; session_reminder_lead_time
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
                  (option Description.t)
                  (tup2
                     (option CostCenter.t)
                     (tup2
                        (option Organisational_unit.Repo.t)
                        (tup2
                           (option Filter.Repo.t)
                           (tup2
                              (option Admin.Repo.Entity.Id.t)
                              (tup2
                                 DirectRegistrationDisabled.t
                                 (tup2
                                    RegistrationDisabled.t
                                    (tup2
                                       AllowUninvitedSignup.t
                                       (tup2
                                          (option
                                             Pool_common.Repo.ExperimentType.t)
                                          (tup2
                                             (option
                                                Pool_common.Repo.Reminder
                                                .LeadTime
                                                .t)
                                             (tup2
                                                Common.Repo.CreatedAt.t
                                                Common.Repo.UpdatedAt.t)))))))))))))))
;;

module Write = struct
  let t =
    let encode (m : t) =
      let filter = m.filter |> CCOption.map (fun filter -> filter.Filter.id) in
      let organisational_unit =
        m.organisational_unit
        |> CCOption.map (fun ou -> ou.Organisational_unit.id)
      in
      Ok
        ( m.id
        , ( m.title
          , ( m.public_title
            , ( m.description
              , ( m.cost_center
                , ( organisational_unit
                  , ( filter
                    , ( m.contact_person_id
                      , ( m.direct_registration_disabled
                        , ( m.registration_disabled
                          , ( m.allow_uninvited_signup
                            , (m.experiment_type, m.session_reminder_lead_time)
                            ) ) ) ) ) ) ) ) ) ) )
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
                    (option Description.t)
                    (tup2
                       (option CostCenter.t)
                       (tup2
                          (option Organisational_unit.Repo.Id.t)
                          (tup2
                             (option RepoId.t)
                             (tup2
                                (option Admin.Repo.Entity.Id.t)
                                (tup2
                                   DirectRegistrationDisabled.t
                                   (tup2
                                      RegistrationDisabled.t
                                      (tup2
                                         AllowUninvitedSignup.t
                                         (tup2
                                            (option
                                               Pool_common.Repo.ExperimentType.t)
                                            (option
                                               Pool_common.Repo.Reminder
                                               .LeadTime
                                               .t))))))))))))))
  ;;
end

module Public = struct
  open Entity.Public

  let t =
    let encode (m : t) =
      Ok
        ( m.id
        , ( m.public_title
          , (m.description, (m.direct_registration_disabled, m.experiment_type))
          ) )
    in
    let decode
      ( id
      , ( public_title
        , (description, (direct_registration_disabled, experiment_type)) ) )
      =
      Ok
        { id
        ; public_title
        ; description
        ; direct_registration_disabled
        ; experiment_type
        }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2
           RepoId.t
           (tup2
              PublicTitle.t
              (tup2
                 (option Description.t)
                 (tup2
                    DirectRegistrationDisabled.t
                    (option Pool_common.Repo.ExperimentType.t))))))
  ;;
end
