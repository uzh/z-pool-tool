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

type t =
  { id : Id.t
  ; title : Title.t
  ; public_title : PublicTitle.t
  ; description : Description.t option
  ; cost_center : CostCenter.t option
  ; organisational_unit : Organisational_unit.t option
  ; filter : Filter.t option
  ; direct_registration_disabled : DirectRegistrationDisabled.t
  ; registration_disabled : RegistrationDisabled.t
  ; allow_uninvited_signup : AllowUninvitedSignup.t
  ; experiment_type : Pool_common.ExperimentType.t option
  ; session_reminder_lead_time : Pool_common.Reminder.LeadTime.t option
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }
[@@deriving eq, show]

let create
  ?id
  title
  public_title
  description
  cost_center
  organisational_unit
  direct_registration_disabled
  registration_disabled
  allow_uninvited_signup
  experiment_type
  session_reminder_lead_time
  =
  let open CCResult in
  Ok
    { id = id |> CCOption.value ~default:(Id.create ())
    ; title
    ; public_title
    ; description
    ; cost_center
    ; organisational_unit
    ; filter = None
    ; direct_registration_disabled
    ; registration_disabled
    ; allow_uninvited_signup
    ; experiment_type
    ; session_reminder_lead_time
    ; created_at = Ptime_clock.now ()
    ; updated_at = Ptime_clock.now ()
    }
;;

let title_value (m : t) = Title.value m.title
let public_title_value (m : t) = PublicTitle.value m.public_title

module Public = struct
  type t =
    { id : Id.t
    ; public_title : PublicTitle.t
    ; description : Description.t option
    ; direct_registration_disabled : DirectRegistrationDisabled.t
    ; experiment_type : Pool_common.ExperimentType.t option
    }
  [@@deriving eq, show]
end

let to_public
  { id
  ; public_title
  ; description
  ; direct_registration_disabled
  ; experiment_type
  ; _
  }
  =
  Public.
    { id
    ; public_title
    ; description
    ; direct_registration_disabled
    ; experiment_type
    }
;;

let session_reminder_lead_time_value m =
  m.session_reminder_lead_time
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

let boolean_fields =
  Pool_common.Message.Field.
    [ DirectRegistrationDisabled; RegistrationDisabled; AllowUninvitedSignup ]
;;

let searchable_by =
  Pool_common.Message.
    [ Field.Title, "pool_experiments.title"
    ; Field.PublicTitle, "pool_experiments.public_title"
    ]
  |> Query.Column.create_list
;;

let sortable_by =
  searchable_by
  @ (Pool_common.Message.[ Field.CreatedAt, "pool_experiments.created_at" ]
     |> Query.Column.create_list)
;;
