module Id = struct
  include Pool_common.Id

  let to_common m = m
end

module Common = Pool_common

module Title = struct
  include Pool_common.Model.String

  let field = Common.Message.Field.Title
  let schema () = schema field ()
end

module PublicTitle = struct
  include Pool_common.Model.String

  let field = Common.Message.Field.PublicTitle
  let schema () = schema field ()
  let placeholder = "###"
end

module Description = struct
  include Pool_common.Model.String

  let field = Common.Message.Field.Description
  let schema () = schema field ()
end

module CostCenter = struct
  include Pool_common.Model.String

  let field = Common.Message.Field.CostCenter
  let schema () = schema field ()
end

module DirectRegistrationDisabled = struct
  include Pool_common.Model.Boolean

  let schema = schema Common.Message.Field.DirectRegistrationDisabled
end

module RegistrationDisabled = struct
  include Pool_common.Model.Boolean

  let schema = schema Common.Message.Field.RegistrationDisabled
end

module AllowUninvitedSignup = struct
  include Pool_common.Model.Boolean

  let schema = schema Common.Message.Field.AllowUninvitedSignup
end

module ExternalDataRequired = struct
  include Pool_common.Model.Boolean

  let schema = schema Common.Message.Field.ExternalDataRequired
end

module ShowExternalDataIdLinks = struct
  include Pool_common.Model.Boolean

  let schema = schema Common.Message.Field.ShowExteralDataIdLinks
end

module InvitationResetAt = struct
  include Pool_common.Model.Ptime

  let create m = Ok m
  let schema = schema Common.Message.Field.InvitationResetAt create
  let of_ptime m = m
end

type t =
  { id : Id.t
  ; title : Title.t
  ; public_title : PublicTitle.t
  ; description : Description.t option
  ; cost_center : CostCenter.t option
  ; organisational_unit : Organisational_unit.t option
  ; filter : Filter.t option
  ; contact_person_id : Admin.Id.t option
  ; smtp_auth_id : Email.SmtpAuth.Id.t option
  ; direct_registration_disabled : DirectRegistrationDisabled.t
  ; registration_disabled : RegistrationDisabled.t
  ; allow_uninvited_signup : AllowUninvitedSignup.t
  ; external_data_required : ExternalDataRequired.t
  ; show_external_data_id_links : ShowExternalDataIdLinks.t
  ; experiment_type : Pool_common.ExperimentType.t option
  ; email_session_reminder_lead_time : Pool_common.Reminder.LeadTime.t option
  ; text_message_session_reminder_lead_time :
      Pool_common.Reminder.LeadTime.t option
  ; invitation_reset_at : InvitationResetAt.t option
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show]

let create
  ?id
  ?contact_person_id
  ?cost_center
  ?description
  ?email_session_reminder_lead_time
  ?experiment_type
  ?filter
  ?invitation_reset_at
  ?organisational_unit
  ?smtp_auth_id
  ?text_message_session_reminder_lead_time
  title
  public_title
  direct_registration_disabled
  registration_disabled
  allow_uninvited_signup
  external_data_required
  show_external_data_id_links
  =
  let open CCResult in
  Ok
    { id = id |> CCOption.value ~default:(Id.create ())
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
    ; created_at = Ptime_clock.now ()
    ; updated_at = Ptime_clock.now ()
    }
;;

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

let text_message_session_reminder_lead_time t =
  t.text_message_session_reminder_lead_time
;;

let invitation_reset_at t = t.invitation_reset_at
let created_at t = t.created_at
let updated_at t = t.updated_at

module DirectEnrollment = struct
  type t =
    { id : Id.t
    ; title : Title.t
    ; public_title : PublicTitle.t
    ; filter : Filter.query option
    ; direct_registration_disabled : DirectRegistrationDisabled.t
    ; registration_disabled : RegistrationDisabled.t
    ; available_spots : bool
    ; matches_filter : bool
    ; contact_already_assigned : bool
    }
  [@@deriving eq, show]

  let assignable
    { available_spots; registration_disabled; contact_already_assigned; _ }
    =
    available_spots
    && (not registration_disabled)
    && not contact_already_assigned
  ;;
end

let title_value (m : t) = Title.value m.title
let public_title_value (m : t) = PublicTitle.value m.public_title

module Public = struct
  type t =
    { id : Id.t
    ; public_title : PublicTitle.t
    ; description : Description.t option
    ; direct_registration_disabled : DirectRegistrationDisabled.t
    ; experiment_type : Pool_common.ExperimentType.t option
    ; smtp_auth_id : Email.SmtpAuth.Id.t option
    }
  [@@deriving eq, show]
end

let to_public
  { id
  ; public_title
  ; description
  ; direct_registration_disabled
  ; experiment_type
  ; smtp_auth_id
  ; _
  }
  =
  Public.
    { id
    ; public_title
    ; description
    ; direct_registration_disabled
    ; experiment_type
    ; smtp_auth_id
    }
;;

let email_session_reminder_lead_time_value m =
  m.email_session_reminder_lead_time
  |> CCOption.map Pool_common.Reminder.LeadTime.value
;;

let text_message_session_reminder_lead_time_value m =
  m.text_message_session_reminder_lead_time
  |> CCOption.map Pool_common.Reminder.LeadTime.value
;;

let direct_registration_disabled_value (m : t) =
  DirectRegistrationDisabled.value m.direct_registration_disabled
;;

let registration_disabled_value (m : t) =
  RegistrationDisabled.value m.registration_disabled
;;

let allow_uninvited_signup_value (m : t) =
  AllowUninvitedSignup.value m.allow_uninvited_signup
;;

let external_data_required_value (m : t) =
  ExternalDataRequired.value m.external_data_required
;;

let show_external_data_id_links_value (m : t) =
  ShowExternalDataIdLinks.value m.show_external_data_id_links
;;

let boolean_fields =
  Pool_common.Message.Field.
    [ DirectRegistrationDisabled
    ; RegistrationDisabled
    ; AllowUninvitedSignup
    ; ExternalDataRequired
    ; ShowExteralDataIdLinks
    ]
;;

let searchable_by =
  Pool_common.Message.
    [ Field.Title, "pool_experiments.title"
    ; Field.PublicTitle, "pool_experiments.public_title"
    ]
  |> Query.Column.create_list
;;

let default_sort_column =
  Pool_common.Message.(Field.CreatedAt, "pool_experiments.created_at")
  |> Query.Column.create
;;

let sortable_by = default_sort_column :: searchable_by

let default_query =
  let open Query in
  let sort =
    Sort.{ column = default_sort_column; order = SortOrder.Descending }
  in
  create ~sort ()
;;
