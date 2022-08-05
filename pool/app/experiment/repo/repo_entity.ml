open Entity
module Common = Pool_common
module Id = Common.Id
module RepoId = Common.Repo.Id

type repo_entity =
  { id : Id.t
  ; title : Title.t
  ; public_title : PublicTitle.t
  ; description : Description.t
  ; filter_id : Id.t option
  ; direct_registration_disabled : DirectRegistrationDisabled.t
  ; registration_disabled : RegistrationDisabled.t
  ; allow_uninvited_signup : AllowUninvitedSignup.t
  ; experiment_type : Pool_common.ExperimentType.t option
  ; invitation_template : InvitationTemplate.t option
  ; session_reminder_lead_time : Pool_common.Reminder.LeadTime.t option
  ; session_reminder_subject : Pool_common.Reminder.Subject.t option
  ; session_reminder_text : Pool_common.Reminder.Text.t option
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }
[@@deriving show]

let to_repo_entity (m : t) =
  let filter_id = CCOption.map (fun f -> f.Filter.id) m.filter in
  ({ id = m.Entity.id
   ; title = m.Entity.title
   ; public_title = m.Entity.public_title
   ; description = m.Entity.description
   ; filter_id
   ; direct_registration_disabled = m.Entity.direct_registration_disabled
   ; registration_disabled = m.Entity.registration_disabled
   ; allow_uninvited_signup = m.Entity.allow_uninvited_signup
   ; experiment_type = m.Entity.experiment_type
   ; invitation_template = m.Entity.invitation_template
   ; session_reminder_lead_time = m.Entity.session_reminder_lead_time
   ; session_reminder_subject = m.Entity.session_reminder_subject
   ; session_reminder_text = m.Entity.session_reminder_text
   ; created_at = m.Entity.created_at
   ; updated_at = m.Entity.updated_at
   }
    : repo_entity)
;;

let of_repo_entity tenant_db (m : repo_entity) =
  let open Lwt_result.Syntax in
  let open Lwt_result.Infix in
  let* filter =
    CCOption.map_or
      ~default:(Lwt_result.return None)
      (fun filter_id -> Filter.find tenant_db filter_id >|= CCOption.pure)
      m.filter_id
  in
  Lwt_result.return
    ({ id = m.id
     ; title = m.title
     ; public_title = m.public_title
     ; description = m.description
     ; filter
     ; direct_registration_disabled = m.direct_registration_disabled
     ; registration_disabled = m.registration_disabled
     ; allow_uninvited_signup = m.allow_uninvited_signup
     ; experiment_type = m.experiment_type
     ; invitation_template = m.invitation_template
     ; session_reminder_lead_time = m.session_reminder_lead_time
     ; session_reminder_subject = m.session_reminder_subject
     ; session_reminder_text = m.session_reminder_text
     ; created_at = m.created_at
     ; updated_at = m.updated_at
     }
      : t)
;;

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

module AllowUninvitedSignup = struct
  include AllowUninvitedSignup

  let t = Caqti_type.bool
end

module InvitationTemplate = struct
  include InvitationTemplate

  module Subject = struct
    include Subject

    let t = Caqti_type.(string)
  end

  module Text = struct
    include Text

    let t = Caqti_type.(string)
  end

  let t =
    let encode (m : t) = Ok (m.subject, m.text) in
    let decode (subject, text) =
      create subject text
      |> CCResult.map_err Common.(Utils.error_to_string Language.En)
    in
    Caqti_type.(custom ~encode ~decode (tup2 Subject.t Text.t))
  ;;
end

let t =
  let encode (m : repo_entity) =
    Ok
      ( m.id
      , ( Title.value m.title
        , ( PublicTitle.value m.public_title
          , ( m.description
            , ( m.filter_id
              , ( m.direct_registration_disabled
                , ( m.registration_disabled
                  , ( m.allow_uninvited_signup
                    , ( m.experiment_type
                      , ( m.invitation_template
                        , ( m.session_reminder_lead_time
                          , ( m.session_reminder_subject
                            , ( m.session_reminder_text
                              , (m.created_at, m.updated_at) ) ) ) ) ) ) ) ) )
            ) ) ) )
  in
  let decode
    ( id
    , ( title
      , ( public_title
        , ( description
          , ( filter_id
            , ( direct_registration_disabled
              , ( registration_disabled
                , ( allow_uninvited_signup
                  , ( experiment_type
                    , ( invitation_template
                      , ( session_reminder_lead_time
                        , ( session_reminder_subject
                          , (session_reminder_text, (created_at, updated_at)) )
                        ) ) ) ) ) ) ) ) ) ) )
    =
    let open CCResult in
    Ok
      { id
      ; title
      ; public_title
      ; description
      ; filter_id
      ; direct_registration_disabled
      ; registration_disabled
      ; allow_uninvited_signup
      ; experiment_type
      ; invitation_template
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
                     (option RepoId.t)
                     (tup2
                        DirectRegistrationDisabled.t
                        (tup2
                           RegistrationDisabled.t
                           (tup2
                              AllowUninvitedSignup.t
                              (tup2
                                 (option Pool_common.Repo.ExperimentType.t)
                                 (tup2
                                    (option InvitationTemplate.t)
                                    (tup2
                                       (option
                                          Pool_common.Repo.Reminder.LeadTime.t)
                                       (tup2
                                          (option
                                             Pool_common.Repo.Reminder.Subject.t)
                                          (tup2
                                             (option
                                                Pool_common.Repo.Reminder.Text.t)
                                             (tup2
                                                Common.Repo.CreatedAt.t
                                                Common.Repo.UpdatedAt.t)))))))))))))))
;;

module Write = struct
  let t =
    let encode (m : repo_entity) =
      Ok
        ( m.id
        , ( Title.value m.title
          , ( PublicTitle.value m.public_title
            , ( Description.value m.description
              , ( m.filter_id
                , ( m.direct_registration_disabled
                  , ( m.registration_disabled
                    , ( m.allow_uninvited_signup
                      , ( m.experiment_type
                        , ( m.invitation_template
                          , ( m.session_reminder_lead_time
                            , ( m.session_reminder_subject
                              , m.session_reminder_text ) ) ) ) ) ) ) ) ) ) ) )
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
                       (option RepoId.t)
                       (tup2
                          DirectRegistrationDisabled.t
                          (tup2
                             RegistrationDisabled.t
                             (tup2
                                AllowUninvitedSignup.t
                                (tup2
                                   (option Pool_common.Repo.ExperimentType.t)
                                   (tup2
                                      (option InvitationTemplate.t)
                                      (tup2
                                         (option
                                            Pool_common.Repo.Reminder.LeadTime.t)
                                         (tup2
                                            (option
                                               Pool_common.Repo.Reminder.Subject
                                               .t)
                                            (option
                                               Pool_common.Repo.Reminder.Text.t))))))))))))))
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
                 Description.t
                 (tup2
                    DirectRegistrationDisabled.t
                    (option Pool_common.Repo.ExperimentType.t))))))
  ;;
end
