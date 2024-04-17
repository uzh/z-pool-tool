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

  let t =
    Common.make_caqti_type
      Caqti_type.string
      (of_string %> CCResult.return)
      value
  ;;
end

module PublicDescription = struct
  include PublicDescription

  let t =
    Common.make_caqti_type
      Caqti_type.string
      (of_string %> CCResult.return)
      value
  ;;
end

module CostCenter = struct
  include CostCenter

  let t = Common.make_caqti_type Caqti_type.string create value
end

module DirectRegistrationDisabled = struct
  include DirectRegistrationDisabled

  let t =
    Common.make_caqti_type Caqti_type.bool (create %> CCResult.return) value
  ;;
end

module RegistrationDisabled = struct
  include RegistrationDisabled

  let t =
    Common.make_caqti_type Caqti_type.bool (create %> CCResult.return) value
  ;;
end

module AllowUninvitedSignup = struct
  include AllowUninvitedSignup

  let t =
    Common.make_caqti_type Caqti_type.bool (create %> CCResult.return) value
  ;;
end

module ExternalDataRequired = struct
  include ExternalDataRequired

  let t =
    Common.make_caqti_type Caqti_type.bool (create %> CCResult.return) value
  ;;
end

module ShowExternalDataIdLinks = struct
  include ShowExternalDataIdLinks

  let t =
    Common.make_caqti_type Caqti_type.bool (create %> CCResult.return) value
  ;;
end

module InvitationResetAt = struct
  include InvitationResetAt

  let t = Common.make_caqti_type Caqti_type.ptime create value
end

module OnlineStudyRepo = struct
  type t =
    { assignment_without_session : AssignmentWithoutSession.t
    ; redirect_immediately : RedirectImmediately.t
    ; survey_url : SurveyUrl.t option
    }

  let t =
    let encode m =
      Ok (m.assignment_without_session, m.redirect_immediately, m.survey_url)
    in
    let decode (assignment_without_session, redirect_immediately, survey_url) =
      Ok { assignment_without_session; redirect_immediately; survey_url }
    in
    Caqti_type.(custom ~encode ~decode (t3 bool bool (option string)))
  ;;
end

let t =
  let encode _ = Pool_common.(Utils.failwith Message.ReadOnlyModel) in
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
                                , ( OnlineStudyRepo.
                                      { assignment_without_session
                                      ; redirect_immediately
                                      ; survey_url
                                      }
                                  , ( email_session_reminder_lead_time
                                    , ( text_message_session_reminder_lead_time
                                      , ( invitation_reset_at
                                        , ( matcher_notification_sent
                                          , ( created_at
                                            , ( updated_at
                                              , (filter, organisational_unit) )
                                            ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) )
        ) ) )
    =
    let open CCResult in
    let online_study =
      OnlineStudy.create_opt
        ~assignment_without_session
        ~redirect_immediately
        ~survey_url
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
      ; online_study
      ; email_session_reminder_lead_time
      ; text_message_session_reminder_lead_time
      ; invitation_reset_at
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
                                                      OnlineStudyRepo.t
                                                      (t2
                                                         (option
                                                            Reminder
                                                            .EmailLeadTime
                                                            .t)
                                                         (t2
                                                            (option
                                                               Reminder
                                                               .TextMessageLeadTime
                                                               .t)
                                                            (t2
                                                               (option
                                                                  InvitationResetAt
                                                                  .t)
                                                               (t2
                                                                  bool
                                                                  (t2
                                                                     CreatedAt.t
                                                                     (t2
                                                                        UpdatedAt
                                                                        .t
                                                                        (t2
                                                                           (option
                                                                              Filter
                                                                              .Repo
                                                                              .t)
                                                                           (option
                                                                              Organisational_unit
                                                                              .Repo
                                                                              .t)))))))))))))))))))))))))
;;

module Write = struct
  let t =
    let encode (m : t) =
      let filter = m.filter |> CCOption.map (fun filter -> filter.Filter.id) in
      let organisational_unit =
        m.organisational_unit
        |> CCOption.map (fun ou -> ou.Organisational_unit.id)
      in
      let online_study =
        let open OnlineStudyRepo in
        match m.online_study with
        | None ->
          { assignment_without_session = false
          ; redirect_immediately = false
          ; survey_url = None
          }
        | Some { OnlineStudy.redirect_immediately; survey_url } ->
          { assignment_without_session = true
          ; redirect_immediately
          ; survey_url = Some survey_url
          }
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
                                        , ( online_study
                                          , ( m.email_session_reminder_lead_time
                                            , ( m
                                                  .text_message_session_reminder_lead_time
                                              , ( m.invitation_reset_at
                                                , m.matcher_notification_sent )
                                              ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) )
            ) ) )
    in
    let decode _ = failwith "Write only model" in
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
                                                        ShowExternalDataIdLinks
                                                        .t
                                                        (t2
                                                           (option
                                                              ExperimentType.t)
                                                           (t2
                                                              OnlineStudyRepo.t
                                                              (t2
                                                                 (option
                                                                    Reminder
                                                                    .EmailLeadTime
                                                                    .t)
                                                                 (t2
                                                                    (option
                                                                       Reminder
                                                                       .TextMessageLeadTime
                                                                       .t)
                                                                    (t2
                                                                       (option
                                                                          InvitationResetAt
                                                                          .t)
                                                                       bool))))))))))))))))))))))
  ;;
end

module Public = struct
  open Entity.Public

  let t =
    let encode _ = Pool_common.(Utils.failwith Message.ReadOnlyModel) in
    let decode
      ( id
      , ( public_title
        , ( description
          , ( language
            , ( direct_registration_disabled
              , ( experiment_type
                , ( smtp_auth_id
                  , OnlineStudyRepo.
                      { assignment_without_session
                      ; redirect_immediately
                      ; survey_url
                      } ) ) ) ) ) ) )
      =
      let online_study =
        OnlineStudy.create_opt
          ~assignment_without_session
          ~redirect_immediately
          ~survey_url
      in
      Ok
        { id
        ; public_title
        ; description
        ; language
        ; direct_registration_disabled
        ; experiment_type
        ; smtp_auth_id
        ; online_study
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
                             OnlineStudyRepo.t))))))))
  ;;
end

module DirectEnrollment = struct
  open Entity.DirectEnrollment

  let t =
    let encode _ =
      failwith
        Pool_common.(Message.ReadOnlyModel |> Utils.error_to_string Language.En)
    in
    let decode
      ( id
      , ( title
        , ( public_title
          , ( filter
            , ( direct_registration_disabled
              , ( registration_disabled
                , (available_spots, contact_already_assigned) ) ) ) ) ) )
      =
      let matches_filter = false in
      Ok
        { id
        ; title
        ; public_title
        ; filter
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
                       DirectRegistrationDisabled.t
                       (t2 RegistrationDisabled.t (t2 bool bool))))))))
  ;;
end
