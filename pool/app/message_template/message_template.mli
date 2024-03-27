module Id : sig
  include module type of Pool_common.Id
end

module Label : sig
  type t =
    | AccountSuspensionNotification
    | AssignmentCancellation
    | AssignmentConfirmation
    | AssignmentSessionChange
    | ContactEmailChangeAttempt
    | ContactRegistrationAttempt
    | EmailVerification
    | ExperimentInvitation
    | ManualSessionMessage
    | PasswordChange
    | PasswordReset
    | PhoneVerification
    | ProfileUpdateTrigger
    | SignUpVerification
    | SessionCancellation
    | SessionReminder
    | SessionReschedule
    | UserImport
    | WaitingListConfirmation

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val read : string -> t
  val read_from_url : string -> t
  val of_string : string -> (t, Pool_message.Error.t) result
  val to_human : t -> string
  val human_url : t -> string
  val prefixed_human_url : t -> string
  val customizable_by_experiment : t list
end

module EmailSubject : sig
  include Pool_model.Base.StringSig
end

module EmailText : sig
  include Pool_model.Base.StringSig
end

module PlainText : sig
  include Pool_model.Base.StringSig
end

module SmsText : sig
  include Pool_model.Base.StringSig
end

module FallbackToEmail : sig
  include Pool_model.Base.BooleanSig
end

type t =
  { id : Id.t
  ; label : Label.t
  ; entity_uuid : Pool_common.Id.t option
  ; language : Pool_common.Language.t
  ; email_subject : EmailSubject.t
  ; email_text : EmailText.t
  ; plain_text : PlainText.t
  ; sms_text : SmsText.t
  }

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val show : t -> string

type update =
  { email_subject : EmailSubject.t
  ; email_text : EmailText.t
  ; plain_text : PlainText.t
  ; sms_text : SmsText.t
  }

module ManualMessage : sig
  type t =
    { recipient : Pool_user.EmailAddress.t
    ; language : Pool_common.Language.t
    ; email_subject : EmailSubject.t
    ; email_text : EmailText.t
    ; plain_text : PlainText.t
    }

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

type event =
  | Created of t
  | Updated of t * update
  | Deleted of t

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val created : t -> event
val updated : t -> update -> event
val deleted : t -> event
val handle_event : Pool_database.Label.t -> event -> unit Lwt.t

val find
  :  Pool_database.Label.t
  -> Id.t
  -> (t, Pool_message.Error.t) result Lwt.t

val find_default_by_label_and_language
  :  Pool_database.Label.t
  -> Pool_common.Language.t
  -> Label.t
  -> t Lwt.t

val find_default_by_label : Pool_database.Label.t -> Label.t -> t list Lwt.t
val all_default : Pool_database.Label.t -> unit -> t list Lwt.t

val find_all_of_entity_by_label
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> Label.t
  -> t list Lwt.t

val find_by_label_and_language_to_send
  :  Pool_database.Label.t
  -> ?entity_uuids:Pool_common.Id.t list
  -> Label.t
  -> Pool_common.Language.t
  -> t Lwt.t

val find_all_by_label_to_send
  :  Pool_database.Label.t
  -> ?entity_uuids:Pool_common.Id.t list
  -> Pool_common.Language.t list
  -> Label.t
  -> t list Lwt.t

val find_entity_defaults_by_label
  :  Pool_database.Label.t
  -> ?entity_uuids:Pool_common.Id.t list
  -> Pool_common.Language.t list
  -> Label.t
  -> t list Lwt.t

val filter_languages
  :  ?exclude:Pool_common.Language.t list
  -> Pool_common.Language.t list
  -> t list
  -> Pool_common.Language.t list

val missing_template_languages
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> Label.t
  -> ?exclude:Pool_common.Language.t list
  -> Pool_common.Language.t list
  -> Pool_common.Language.t list Lwt.t

type layout =
  | Tenant of Pool_tenant.t
  | Root

val to_human_label : t -> string
val prefixed_template_url : ?append:string -> t -> string

module Guard : sig
  module Target : sig
    val to_authorizable
      :  ?ctx:(string * string) list
      -> t
      -> (Guard.Target.t, Pool_message.Error.t) Lwt_result.t

    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module Access : sig
    val index : Guard.ValidationSet.t
    val create : Guard.ValidationSet.t
    val update : Id.t -> Guard.ValidationSet.t
    val delete : Id.t -> Guard.ValidationSet.t
  end
end

val create_public_url_with_params
  :  Pool_tenant.Url.t
  -> string
  -> (Pool_message.Field.t * string) list
  -> string

type email_layout =
  { link : string
  ; logo_alt : string
  ; logo_src : string
  ; site_title : string
  }

val layout_from_tenant : Pool_tenant.t -> email_layout
val template_hint : Label.t -> Pool_common.I18n.hint

module AccountSuspensionNotification : sig
  val email_params : email_layout -> Sihl_user.t -> (string * string) list

  val create
    :  Pool_tenant.t
    -> Sihl_user.t
    -> (Email.job, Pool_message.Error.t) result Lwt.t
end

module AssignmentCancellation : sig
  val email_params
    :  ?follow_up_sessions:Session.t list
    -> Pool_common.Language.t
    -> email_layout
    -> Experiment.t
    -> Session.t
    -> Assignment.t
    -> (string * string) list

  val create
    :  ?follow_up_sessions:Session.t list
    -> Pool_tenant.t
    -> Experiment.t
    -> Session.t
    -> Assignment.t
    -> Admin.t option
    -> Email.job Lwt.t
end

module AssignmentConfirmation : sig
  val email_params
    :  ?follow_up_sessions:Session.t list
    -> Pool_common.Language.t
    -> email_layout
    -> Experiment.t
    -> Session.t
    -> Assignment.t
    -> (string * string) list

  val prepare
    :  ?follow_up_sessions:Session.t list
    -> Pool_tenant.t
    -> Contact.t
    -> Experiment.t
    -> Session.t
    -> Admin.t option
    -> (Assignment.t -> Email.job) Lwt.t
end

module AssignmentSessionChange : sig
  val email_params
    :  Pool_common.Language.t
    -> email_layout
    -> Experiment.t
    -> new_session:Session.t
    -> old_session:Session.t
    -> Assignment.t
    -> (string * string) list

  val create
    :  ManualMessage.t
    -> Pool_tenant.t
    -> Experiment.t
    -> new_session:Session.t
    -> old_session:Session.t
    -> Assignment.t
    -> Email.job Lwt.t
end

module ContactEmailChangeAttempt : sig
  val email_params
    :  email_layout
    -> Pool_tenant.Url.t
    -> Sihl_user.t
    -> (string * string) list

  val create
    :  Pool_tenant.t
    -> Sihl_user.t
    -> (Email.job, Pool_message.Error.t) Lwt_result.t
end

module ContactRegistrationAttempt : sig
  val email_params
    :  email_layout
    -> Pool_tenant.Url.t
    -> Sihl_user.t
    -> (string * string) list

  val create
    :  Pool_common.Language.t
    -> Pool_tenant.t
    -> Sihl_user.t
    -> Email.job Lwt.t
end

module EmailVerification : sig
  val email_params
    :  email_layout
    -> string
    -> Contact.t
    -> (string * string) list

  val create
    :  Pool_database.Label.t
    -> Pool_common.Language.t
    -> layout
    -> Contact.t
    -> Pool_user.EmailAddress.t
    -> Email.Token.t
    -> Email.job Lwt.t
end

module ExperimentInvitation : sig
  val email_params
    :  email_layout
    -> Experiment.t
    -> Pool_tenant.Url.t
    -> Contact.t
    -> (string * string) list

  val create : Pool_tenant.t -> Experiment.t -> Contact.t -> Email.job Lwt.t

  val prepare
    :  Pool_tenant.t
    -> Experiment.t
    -> (Contact.t -> (Email.job, Pool_message.Error.t) result) Lwt.t
end

module ManualSessionMessage : sig
  val email_params
    :  Pool_common.Language.t
    -> email_layout
    -> Experiment.t
    -> Session.t
    -> Assignment.t
    -> (string * string) list

  val prepare
    :  Pool_tenant.t
    -> Session.t
    -> (Assignment.t -> ManualMessage.t -> Email.job) Lwt.t

  val prepare_text_message
    :  Pool_tenant.t
    -> Session.t
    -> Pool_common.Language.t
    -> Assignment.t
    -> SmsText.t
    -> Pool_user.CellPhone.t
    -> Text_message.job
end

module PasswordChange : sig
  val email_params : email_layout -> Sihl_user.t -> (string * string) list

  val create
    :  Pool_common.Language.t
    -> Pool_tenant.t
    -> Sihl_user.t
    -> Email.job Lwt.t
end

module PasswordReset : sig
  val email_params
    :  email_layout
    -> string
    -> Sihl_user.t
    -> (string * string) list

  val create
    :  Pool_database.Label.t
    -> Pool_common.Language.t
    -> layout
    -> Sihl_user.t
    -> (Email.job, Pool_message.Error.t) result Lwt.t
end

module PhoneVerification : sig
  val message_params : Pool_common.VerificationCode.t -> (string * string) list

  val create_text_message
    :  Pool_database.Label.t
    -> Pool_common.Language.t
    -> Pool_tenant.t
    -> Contact.t
    -> Pool_user.CellPhone.t
    -> Pool_common.VerificationCode.t
    -> (Text_message.job, Pool_message.Error.t) result Lwt.t
end

module ProfileUpdateTrigger : sig
  val email_params
    :  email_layout
    -> Pool_tenant.Url.t
    -> Contact.t
    -> (string * string) list

  val prepare
    :  Pool_database.Label.t
    -> Pool_tenant.t
    -> (Contact.t -> (Email.job, Pool_message.Error.t) result) Lwt.t
end

module SessionCancellation : sig
  val email_params
    :  Pool_common.Language.t
    -> email_layout
    -> Experiment.t
    -> Session.t
    -> Session.t list
    -> Session.CancellationReason.t
    -> Contact.t
    -> (string * string) list

  val prepare
    :  Pool_database.Label.t
    -> Pool_tenant.t
    -> Experiment.t
    -> Pool_common.Language.t list
    -> Session.t
    -> Session.t list
    -> (Session.CancellationReason.t
        -> Contact.t
        -> (Email.job, Pool_message.Error.t) result)
         Lwt.t

  val prepare_text_message
    :  Pool_database.Label.t
    -> Pool_tenant.t
    -> Experiment.t
    -> Pool_common.Language.t list
    -> Session.t
    -> Session.t list
    -> (Session.CancellationReason.t
        -> Contact.t
        -> Pool_user.CellPhone.t
        -> (Text_message.job, Pool_message.Error.t) result)
         Lwt.t
end

module SessionReminder : sig
  val email_params
    :  Pool_common.Language.t
    -> email_layout
    -> Experiment.t
    -> Session.t
    -> Assignment.t
    -> (string * string) list

  val create
    :  Pool_database.Label.t
    -> Pool_tenant.t
    -> Pool_common.Language.t list
    -> Experiment.t
    -> Session.t
    -> Assignment.t
    -> Email.job Lwt.t

  val prepare_emails
    :  Pool_database.Label.t
    -> Pool_tenant.t
    -> Pool_common.Language.t list
    -> Experiment.t
    -> Session.t
    -> (Assignment.t -> (Email.job, Pool_message.Error.t) result) Lwt.t

  val prepare_text_messages
    :  Pool_database.Label.t
    -> Pool_tenant.t
    -> Pool_common.Language.t list
    -> Experiment.t
    -> Session.t
    -> (Assignment.t
        -> Pool_user.CellPhone.t
        -> (Text_message.job, Pool_message.Error.t) result)
         Lwt.t
end

module SessionReschedule : sig
  val email_params
    :  Pool_common.Language.t
    -> email_layout
    -> Experiment.t
    -> Session.t
    -> Session.Start.t
    -> Session.Duration.t
    -> Contact.t
    -> (string * string) list

  val prepare
    :  Pool_database.Label.t
    -> Pool_tenant.t
    -> Experiment.t
    -> Pool_common.Language.t list
    -> Session.t
    -> Admin.t option
    -> (Contact.t
        -> Session.Start.t
        -> Session.Duration.t
        -> (Email.job, Pool_message.Error.t) result)
         Lwt.t
end

module SignUpVerification : sig
  val email_params
    :  email_layout
    -> string
    -> Pool_user.Firstname.t
    -> Pool_user.Lastname.t
    -> (string * string) list

  val create
    :  Pool_database.Label.t
    -> Pool_common.Language.t
    -> Pool_tenant.t
    -> Pool_user.EmailAddress.t
    -> Email.Token.t
    -> Pool_user.Firstname.t
    -> Pool_user.Lastname.t
    -> Pool_common.Id.t
    -> Email.job Lwt.t
end

module UserImport : sig
  val email_params
    :  email_layout
    -> string
    -> [< `Admin of Admin.t | `Contact of Contact.t ]
    -> (string * string) list

  val prepare
    :  Pool_database.Label.t
    -> Pool_tenant.t
    -> ([< `Admin of Admin.t | `Contact of Contact.t ] -> string -> Email.job)
         Lwt.t
end

module WaitingListConfirmation : sig
  val email_params
    :  email_layout
    -> Contact.t
    -> Experiment.Public.t
    -> (string * string) list

  val create
    :  Pool_tenant.t
    -> Contact.t
    -> Experiment.Public.t
    -> (Email.job, Pool_message.Error.t) result Lwt.t
end

val sms_text_to_email : SmsText.t -> EmailText.t * PlainText.t

val experiment_message_language
  :  Pool_common.Language.t list
  -> Experiment.t
  -> Contact.t
  -> Pool_common.Language.t
