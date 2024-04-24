module Id = struct
  include Pool_common.Id

  let to_common m = m
end

module Title = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.Title
  let schema () = schema field ()
end

module PublicTitle = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.PublicTitle
  let schema () = schema field ()
  let placeholder = "###"
end

module InternalDescription = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.InternalDescription
  let schema () = schema field ()
end

module PublicDescription = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.PublicDescription
  let schema () = schema field ()
end

module CostCenter = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.CostCenter
  let schema () = schema field ()
end

module DirectRegistrationDisabled = struct
  include Pool_model.Base.Boolean

  let schema = schema Pool_message.Field.DirectRegistrationDisabled
end

module RegistrationDisabled = struct
  include Pool_model.Base.Boolean

  let schema = schema Pool_message.Field.RegistrationDisabled
end

module AllowUninvitedSignup = struct
  include Pool_model.Base.Boolean

  let schema = schema Pool_message.Field.AllowUninvitedSignup
end

module ExternalDataRequired = struct
  include Pool_model.Base.Boolean

  let schema = schema Pool_message.Field.ExternalDataRequired
end

module ShowExternalDataIdLinks = struct
  include Pool_model.Base.Boolean

  let schema = schema Pool_message.Field.ShowExteralDataIdLinks
end

module InvitationResetAt = struct
  include Pool_model.Base.Ptime

  let create m = Ok m
  let schema = schema Pool_message.Field.InvitationResetAt create
  let of_ptime m = m
end

type t =
  { id : Id.t
  ; title : Title.t
  ; public_title : PublicTitle.t
  ; internal_description : InternalDescription.t option
  ; public_description : PublicDescription.t option
  ; language : Pool_common.Language.t option
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
  ; email_session_reminder_lead_time :
      Pool_common.Reminder.EmailLeadTime.t option
  ; text_message_session_reminder_lead_time :
      Pool_common.Reminder.TextMessageLeadTime.t option
  ; invitation_reset_at : InvitationResetAt.t option
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, fields ~getters, show]

let create
  ?id
  ?contact_person_id
  ?cost_center
  ?internal_description
  ?public_description
  ?language
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
    ; internal_description
    ; public_description
    ; language
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
    ; created_at = Pool_common.CreatedAt.create_now ()
    ; updated_at = Pool_common.UpdatedAt.create_now ()
    }
;;

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

  let assignable { available_spots; contact_already_assigned; _ } =
    available_spots && not contact_already_assigned
  ;;
end

let title_value (m : t) = Title.value m.title
let public_title_value (m : t) = PublicTitle.value m.public_title

module Public = struct
  type t =
    { id : Id.t
    ; public_title : PublicTitle.t
    ; description : PublicDescription.t option
    ; language : Pool_common.Language.t option
    ; direct_registration_disabled : DirectRegistrationDisabled.t
    ; experiment_type : Pool_common.ExperimentType.t option
    ; smtp_auth_id : Email.SmtpAuth.Id.t option
    }
  [@@deriving eq, show]

  let create
    ?description
    ?language
    ?experiment_type
    ?smtp_auth_id
    id
    public_title
    direct_registration_disabled
    =
    { id
    ; public_title
    ; description
    ; language
    ; direct_registration_disabled
    ; experiment_type
    ; smtp_auth_id
    }
  ;;

  let update_direct_registration_disabled (m : t) direct_registration_disabled =
    { m with direct_registration_disabled }
  ;;

  let id (m : t) = m.id
  let public_title (m : t) = m.public_title
  let description (m : t) = m.description
  let language (m : t) = m.language
  let direct_registration_disabled (m : t) = m.direct_registration_disabled
  let experiment_type (m : t) = m.experiment_type
  let smtp_auth_id (m : t) = m.smtp_auth_id
end

let to_public
  { id
  ; public_title
  ; public_description
  ; language
  ; direct_registration_disabled
  ; experiment_type
  ; smtp_auth_id
  ; _
  }
  =
  { Public.id
  ; public_title
  ; description = public_description
  ; language
  ; direct_registration_disabled
  ; experiment_type
  ; smtp_auth_id
  }
;;

let email_session_reminder_lead_time_value m =
  m.email_session_reminder_lead_time
  |> CCOption.map Pool_common.Reminder.EmailLeadTime.value
;;

let text_message_session_reminder_lead_time_value m =
  m.text_message_session_reminder_lead_time
  |> CCOption.map Pool_common.Reminder.TextMessageLeadTime.value
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
  Pool_message.Field.
    [ DirectRegistrationDisabled
    ; RegistrationDisabled
    ; AllowUninvitedSignup
    ; ExternalDataRequired
    ; ShowExteralDataIdLinks
    ]
;;

open Pool_message

let column_title =
  (Field.Title, "pool_experiments.title") |> Query.Column.create
;;

let column_public_title =
  (Field.PublicTitle, "pool_experiments.public_title") |> Query.Column.create
;;

let column_created_at =
  (Field.CreatedAt, "pool_experiments.created_at") |> Query.Column.create
;;

let column_experiment_type =
  (Field.ExperimentType, "pool_experiments.experiment_type")
  |> Query.Column.create
;;

let experiment_type_filter =
  let open Query.Filter in
  let open Pool_common.ExperimentType in
  let languages = Pool_common.Language.all in
  let options =
    all
    |> CCList.map (fun exp_type ->
      let label =
        languages
        |> CCList.map (fun lang ->
          lang, show exp_type |> CCString.capitalize_ascii)
      in
      let value = show exp_type in
      SelectOption.create label value)
  in
  Condition.Human.Select (column_experiment_type, options)
;;

let filterable_by = Some [ experiment_type_filter ]
let searchable_by = [ column_title; column_public_title; Tags.column_title ]
let sortable_by = column_created_at :: searchable_by

let default_sort =
  Query.Sort.{ column = column_created_at; order = SortOrder.Descending }
;;

let default_query = Query.create ~sort:default_sort ()
