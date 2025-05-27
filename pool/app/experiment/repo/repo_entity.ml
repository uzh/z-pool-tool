open CCFun
open Entity
module Common = Pool_common.Repo
module Id = Common.Id

module Title = struct
  include Title

  let t = Common.make_caqti_type Caqti_type.string create value
end

module PublicTitle = struct
  include PublicTitle

  let t = Common.make_caqti_type Caqti_type.string create value
end

module InternalDescription = struct
  include InternalDescription

  let t = Common.make_caqti_type Caqti_type.string (of_string %> CCResult.return) value
end

module PublicDescription = struct
  include PublicDescription

  let t = Common.make_caqti_type Caqti_type.string (of_string %> CCResult.return) value
end

module CostCenter = struct
  include CostCenter

  let t = Common.make_caqti_type Caqti_type.string create value
end

module DirectRegistrationDisabled = struct
  include DirectRegistrationDisabled

  let t = Common.make_caqti_type Caqti_type.bool (create %> CCResult.return) value
end

module RegistrationDisabled = struct
  include RegistrationDisabled

  let t = Common.make_caqti_type Caqti_type.bool (create %> CCResult.return) value
end

module AllowUninvitedSignup = struct
  include AllowUninvitedSignup

  let t = Common.make_caqti_type Caqti_type.bool (create %> CCResult.return) value
end

module ExternalDataRequired = struct
  include ExternalDataRequired

  let t = Common.make_caqti_type Caqti_type.bool (create %> CCResult.return) value
end

module ShowExternalDataIdLinks = struct
  include ShowExternalDataIdLinks

  let t = Common.make_caqti_type Caqti_type.bool (create %> CCResult.return) value
end

module OnlineExperimentRepo = struct
  type t =
    { assignment_without_session : AssignmentWithoutSession.t
    ; survey_url : SurveyUrl.t option
    }

  let t =
    let encode m = Ok (m.assignment_without_session, m.survey_url) in
    let decode (assignment_without_session, survey_url) =
      Ok { assignment_without_session; survey_url }
    in
    Caqti_type.(custom ~encode ~decode (t2 bool (option string)))
  ;;
end

let t =
  let encode _ = Pool_common.Utils.failwith Pool_message.Error.ReadOnlyModel in
  let decode
        ( id
        , ( title
          , ( public_title
            , ( internal_description
              , ( public_description
                , ( language
                  , ( cost_center
                    , ( contact_email
                      , ( smtp_auth_id
                        , ( direct_registration_disabled
                          , ( registration_disabled
                            , ( allow_uninvited_signup
                              , ( external_data_required
                                , ( show_external_data_id_links
                                  , ( experiment_type
                                    , ( OnlineExperimentRepo.
                                          { assignment_without_session; survey_url }
                                      , ( email_session_reminder_lead_time
                                        , ( text_message_session_reminder_lead_time
                                          , ( matcher_notification_sent
                                            , ( created_at
                                              , (updated_at, (filter, organisational_unit))
                                              ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) )
    =
    let open CCResult in
    let online_experiment =
      OnlineExperiment.create_opt ~assignment_without_session ~survey_url
    in
    Ok
      { id
      ; title
      ; public_title
      ; internal_description
      ; public_description
      ; language
      ; cost_center
      ; organisational_unit
      ; filter
      ; contact_email
      ; smtp_auth_id
      ; direct_registration_disabled
      ; registration_disabled
      ; allow_uninvited_signup
      ; external_data_required
      ; show_external_data_id_links
      ; experiment_type
      ; online_experiment
      ; email_session_reminder_lead_time
      ; text_message_session_reminder_lead_time
      ; matcher_notification_sent
      ; created_at
      ; updated_at
      }
  in
  let open Common in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (t2
         Id.t
         (t2
            Title.t
            (t2
               PublicTitle.t
               (t2
                  (option InternalDescription.t)
                  (t2
                     (option PublicDescription.t)
                     (t2
                        (option Pool_common.Repo.Language.t)
                        (t2
                           (option CostCenter.t)
                           (t2
                              (option Pool_user.Repo.EmailAddress.t)
                              (t2
                                 (option Email.SmtpAuth.RepoEntity.Id.t)
                                 (t2
                                    DirectRegistrationDisabled.t
                                    (t2
                                       RegistrationDisabled.t
                                       (t2
                                          AllowUninvitedSignup.t
                                          (t2
                                             ExternalDataRequired.t
                                             (t2
                                                ShowExternalDataIdLinks.t
                                                (t2
                                                   (option ExperimentType.t)
                                                   (t2
                                                      OnlineExperimentRepo.t
                                                      (t2
                                                         (option Reminder.EmailLeadTime.t)
                                                         (t2
                                                            (option
                                                               Reminder
                                                               .TextMessageLeadTime
                                                               .t)
                                                            (t2
                                                               bool
                                                               (t2
                                                                  CreatedAt.t
                                                                  (t2
                                                                     UpdatedAt.t
                                                                     (t2
                                                                        (option
                                                                           Filter.Repo.t)
                                                                        (option
                                                                           Organisational_unit
                                                                           .Repo
                                                                           .t))))))))))))))))))))))))
;;

module Write = struct
  let t =
    let encode (m : t) =
      let filter = m.filter |> CCOption.map (fun filter -> filter.Filter.id) in
      let organisational_unit =
        m.organisational_unit |> CCOption.map (fun ou -> ou.Organisational_unit.id)
      in
      let online_experiment =
        let open OnlineExperimentRepo in
        match m.online_experiment with
        | None -> { assignment_without_session = false; survey_url = None }
        | Some { OnlineExperiment.survey_url } ->
          { assignment_without_session = true; survey_url = Some survey_url }
      in
      Ok
        ( m.id
        , ( m.title
          , ( m.public_title
            , ( m.internal_description
              , ( m.public_description
                , ( m.language
                  , ( m.cost_center
                    , ( organisational_unit
                      , ( filter
                        , ( m.contact_email
                          , ( m.smtp_auth_id
                            , ( m.direct_registration_disabled
                              , ( m.registration_disabled
                                , ( m.allow_uninvited_signup
                                  , ( m.external_data_required
                                    , ( m.show_external_data_id_links
                                      , ( m.experiment_type
                                        , ( online_experiment
                                          , ( m.email_session_reminder_lead_time
                                            , ( m.text_message_session_reminder_lead_time
                                              , m.matcher_notification_sent ) ) ) ) ) ) )
                                ) ) ) ) ) ) ) ) ) ) ) ) )
    in
    let decode _ = Pool_common.Utils.failwith Pool_message.Error.WriteOnlyModel in
    let open Common in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (t2
           Id.t
           (t2
              Title.t
              (t2
                 PublicTitle.t
                 (t2
                    (option InternalDescription.t)
                    (t2
                       (option PublicDescription.t)
                       (t2
                          (option Pool_common.Repo.Language.t)
                          (t2
                             (option CostCenter.t)
                             (t2
                                (option Organisational_unit.Repo.Id.t)
                                (t2
                                   (option Id.t)
                                   (t2
                                      (option Pool_user.Repo.EmailAddress.t)
                                      (t2
                                         (option Email.SmtpAuth.RepoEntity.Id.t)
                                         (t2
                                            DirectRegistrationDisabled.t
                                            (t2
                                               RegistrationDisabled.t
                                               (t2
                                                  AllowUninvitedSignup.t
                                                  (t2
                                                     ExternalDataRequired.t
                                                     (t2
                                                        ShowExternalDataIdLinks.t
                                                        (t2
                                                           (option ExperimentType.t)
                                                           (t2
                                                              OnlineExperimentRepo.t
                                                              (t2
                                                                 (option
                                                                    Reminder.EmailLeadTime
                                                                    .t)
                                                                 (t2
                                                                    (option
                                                                       Reminder
                                                                       .TextMessageLeadTime
                                                                       .t)
                                                                    bool)))))))))))))))))))))
  ;;
end

module Public = struct
  open Entity.Public

  let t =
    let encode _ = Pool_common.Utils.failwith Pool_message.Error.ReadOnlyModel in
    let decode
          ( id
          , ( public_title
            , ( description
              , ( language
                , ( direct_registration_disabled
                  , ( experiment_type
                    , ( smtp_auth_id
                      , OnlineExperimentRepo.{ assignment_without_session; survey_url } )
                    ) ) ) ) ) )
      =
      let online_experiment =
        OnlineExperiment.create_opt ~assignment_without_session ~survey_url
      in
      Ok
        { id
        ; public_title
        ; description
        ; language
        ; direct_registration_disabled
        ; experiment_type
        ; smtp_auth_id
        ; online_experiment
        }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (t2
           Id.t
           (t2
              PublicTitle.t
              (t2
                 (option PublicDescription.t)
                 (t2
                    (option Pool_common.Repo.Language.t)
                    (t2
                       DirectRegistrationDisabled.t
                       (t2
                          (option Common.ExperimentType.t)
                          (t2
                             (option Email.SmtpAuth.RepoEntity.Id.t)
                             OnlineExperimentRepo.t))))))))
  ;;
end

module DirectEnrollment = struct
  open Entity.DirectEnrollment

  let t =
    let encode _ = Pool_message.Error.ReadOnlyModel |> Pool_common.Utils.failwith in
    let decode
          ( id
          , ( title
            , ( public_title
              , ( filter
                , ( allow_uninvited_signup
                  , ( direct_registration_disabled
                    , (registration_disabled, (available_spots, contact_already_assigned))
                    ) ) ) ) ) )
      =
      let matches_filter = false in
      Ok
        { id
        ; title
        ; public_title
        ; filter
        ; allow_uninvited_signup
        ; direct_registration_disabled
        ; registration_disabled
        ; available_spots
        ; matches_filter
        ; contact_already_assigned
        }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (t2
           Id.t
           (t2
              Title.t
              (t2
                 PublicTitle.t
                 (t2
                    (option Filter.Repo.query)
                    (t2
                       AllowUninvitedSignup.t
                       (t2
                          DirectRegistrationDisabled.t
                          (t2 RegistrationDisabled.t (t2 bool bool)))))))))
  ;;
end
