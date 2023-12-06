open Pool_common

type experiment_type = Pool_common.ExperimentType.t =
  | Lab
  | Online
[@@deriving eq, show]

type t =
  { id : Id.t
  ; title : string
  ; public_title : string
  ; description : string option
  ; cost_center : string option
  ; organisational_unit : Organisational_unit.t option
  ; filter : Filter.t option
  ; contact_person_id : Id.t option
  ; smtp_auth_id : Id.t option
  ; direct_registration_disabled : bool
  ; registration_disabled : bool
  ; allow_uninvited_signup : bool
  ; external_data_required : bool
  ; show_external_data_id_links : bool
  ; experiment_type : experiment_type
  ; email_session_reminder_lead_time : Non_negative_time_span.t option
  ; text_message_session_reminder_lead_time : Non_negative_time_span.t option
  ; invitation_reset_at : Time_span.t option
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }
[@@deriving eq, show]

let id t = t.id
let title t = t.title
let public_title t = t.public_title
let description t = t.description
let cost_center t = t.cost_center
let organisational_unit t = t.organisational_unit
let filter t = t.filter
let contact_person_id t = t.contact_person_id
let smtp_auth_id t = t.smtp_auth_id
let direct_registration_disabled t = t.direct_registration_disabled
let registration_disabled t = t.registration_disabled
let allow_uninvited_signup t = t.allow_uninvited_signup
let external_data_required t = t.external_data_required
let show_external_data_id_links t = t.show_external_data_id_links
let experiment_type t = t.experiment_type
let email_session_reminder_lead_time t = t.email_session_reminder_lead_time
let invitation_reset_at t = t.invitation_reset_at
let created_at t = t.created_at
let updated_at t = t.updated_at

let text_message_session_reminder_lead_time t =
  t.text_message_session_reminder_lead_time
;;

let public_title_placeholder = "###"

let sql_select_fragment =
  [%string
    {sql|
        %{ Id.sql_select_fragment ~field:"pool_experiments.contact_person_uuid" },
        %{ Id.sql_select_fragment ~field:"pool_experiments.smtp_auth_uuid" },
        %{ Id.sql_select_fragment ~field:"pool_experiments.uuid" },
        pool_experiments.allow_uninvited_signup,
        pool_experiments.cost_center,
        pool_experiments.created_at,
        pool_experiments.description,
        pool_experiments.direct_registration_disabled,
        pool_experiments.email_session_reminder_lead_time,
        pool_experiments.experiment_type,
        pool_experiments.external_data_required,
        pool_experiments.invitation_reset_at,
        pool_experiments.public_title,
        pool_experiments.registration_disabled,
        pool_experiments.show_external_data_id_links,
        pool_experiments.text_message_session_reminder_lead_time,
        pool_experiments.title,
        pool_experiments.updated_at,
        %{ Id.sql_select_fragment ~field:"pool_filter.uuid" },
        pool_filter.created_at,
        pool_filter.query,
        pool_filter.title,
        pool_filter.updated_at,
        %{ Organisational_unit.sql_select_fragment }
|sql}]
;;

let schema =
  let open Caqti_type in
  let module Id = Pool_common.Repo.Id in
  let decode
    ( id
    , ( title
      , ( public_title
        , ( description
          , ( cost_center
            , ( organisational_unit
              , ( filter
                , ( contact_person_id
                  , ( smtp_auth_id
                    , ( direct_registration_disabled
                      , ( registration_disabled
                        , ( allow_uninvited_signup
                          , ( external_data_required
                            , ( show_external_data_id_links
                              , ( experiment_type
                                , ( email_session_reminder_lead_time
                                  , ( text_message_session_reminder_lead_time
                                    , ( invitation_reset_at
                                      , (created_at, (updated_at, ())) ) ) ) )
                              ) ) ) ) ) ) ) ) ) ) ) ) ) )
    =
    Ok
      { id
      ; title
      ; public_title
      ; description
      ; cost_center
      ; organisational_unit
      ; filter
      ; contact_person_id
      ; smtp_auth_id
      ; direct_registration_disabled
      ; registration_disabled
      ; allow_uninvited_signup
      ; external_data_required
      ; show_external_data_id_links
      ; experiment_type
      ; email_session_reminder_lead_time
      ; text_message_session_reminder_lead_time
      ; invitation_reset_at
      ; created_at
      ; updated_at
      }
  in
  let encode t : ('a Caqti_encoders.Data.t, string) result =
    Ok
      Caqti_encoders.Data.
        [ t.id
        ; t.title
        ; t.public_title
        ; t.description
        ; t.cost_center
        ; t.organisational_unit
        ; t.filter
        ; t.contact_person_id
        ; t.smtp_auth_id
        ; t.direct_registration_disabled
        ; t.registration_disabled
        ; t.allow_uninvited_signup
        ; t.external_data_required
        ; t.show_external_data_id_links
        ; t.experiment_type
        ; t.email_session_reminder_lead_time
        ; t.text_message_session_reminder_lead_time
        ; t.invitation_reset_at
        ; t.created_at
        ; t.updated_at
        ]
  in
  Caqti_encoders.(
    custom
      ~encode
      ~decode
      Schema.
        [ Id.t
        ; string
        ; string
        ; option string
        ; option string
        ; option Organisational_unit.Repo.t
        ; option Filter.Repo.t
        ; option Id.t
        ; option Id.t
        ; bool
        ; bool
        ; bool
        ; bool
        ; bool
        ; Pool_common.Repo.ExperimentType.t
        ; option Non_negative_time_span.schema
        ; option Non_negative_time_span.schema
        ; option Time_span.schema
        ; ptime
        ; ptime
        ])
;;
