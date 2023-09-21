module Id : sig
  include module type of Pool_common.Id
end

module Label : sig
  type t =
    | AccountSuspensionNotification
    | AssignmentConfirmation
    | ContactRegistrationAttempt
    | EmailVerification
    | ExperimentInvitation
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
  val of_string : string -> (t, Pool_common.Message.error) result
  val to_human : t -> string
  val human_url : t -> string
  val prefixed_human_url : t -> string
  val customizable_by_experiment : t list
end

module EmailSubject : sig
  include Pool_common.Model.StringSig
end

module EmailText : sig
  include Pool_common.Model.StringSig
end

module PlainText : sig
  include Pool_common.Model.StringSig
end

module SmsText : sig
  include Pool_common.Model.StringSig
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

type event =
  | Created of t
  | DefaultRestored of t list
  | Updated of t * update
  | Deleted of t

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val created : t -> event
val defaultrestored : t list -> event
val updated : t -> update -> event
val deleted : t -> event
val handle_event : Pool_database.Label.t -> event -> unit Lwt.t
val default_values_tenant : t list
val default_values_root : t list

val find
  :  Pool_database.Label.t
  -> Id.t
  -> (t, Pool_common.Message.error) result Lwt.t

val all_default : Pool_database.Label.t -> unit -> t list Lwt.t

val find_all_of_entity_by_label
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> Label.t
  -> t list Lwt.t

val find_by_label_to_send
  :  Pool_database.Label.t
  -> ?entity_uuids:Pool_common.Id.t list
  -> Pool_common.Language.t
  -> Label.t
  -> (t * Pool_common.Language.t) Lwt.t

val find_all_by_label_to_send
  :  Pool_database.Label.t
  -> ?entity_uuids:Pool_common.Id.t list
  -> Pool_common.Language.t list
  -> Label.t
  -> t list Lwt.t

val filter_languages
  :  Pool_common.Language.t list
  -> t list
  -> Pool_common.Language.t list

val find_available_languages
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> Label.t
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
      -> (Guard.Target.t, Pool_common.Message.error) Lwt_result.t

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
  -> (Pool_common.Message.Field.t * string) list
  -> string

type email_layout =
  { link : string
  ; logo_alt : string
  ; logo_src : string
  ; site_title : string
  }

val layout_from_tenant : Pool_tenant.t -> email_layout

module AccountSuspensionNotification : sig
  val email_params : email_layout -> Sihl_user.t -> (string * string) list

  val create
    :  Pool_tenant.t
    -> Sihl_user.t
    -> (Sihl_email.t, Pool_common.Message.error) result Lwt.t
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
    -> Pool_database.Label.t
    -> Pool_common.Language.t
    -> Pool_tenant.t
    -> Experiment.t
    -> Session.t
    -> Admin.t option
    -> (Assignment.t -> Sihl_email.t) Lwt.t
end

module ContactRegistrationAttempt : sig
  val email_params
    :  email_layout
    -> Pool_tenant.Url.t
    -> Contact.t
    -> (string * string) list

  val create
    :  Pool_database.Label.t
    -> Pool_common.Language.t
    -> Pool_tenant.t
    -> Contact.t
    -> Sihl_email.t Lwt.t
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
    -> Sihl_email.t Lwt.t
end

module ExperimentInvitation : sig
  val email_params
    :  email_layout
    -> Experiment.t
    -> Pool_tenant.Url.t
    -> Contact.t
    -> (string * string) list

  val create : Pool_tenant.t -> Experiment.t -> Contact.t -> Sihl_email.t Lwt.t

  val prepare
    :  Pool_tenant.t
    -> Experiment.t
    -> (Contact.t -> (Sihl_email.t, Pool_common.Message.error) result) Lwt.t
end

module PasswordChange : sig
  val email_params : email_layout -> Sihl_user.t -> (string * string) list

  val create
    :  Pool_database.Label.t
    -> Pool_common.Language.t
    -> Pool_tenant.t
    -> Sihl_user.t
    -> Sihl_email.t Lwt.t
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
    -> (Sihl_email.t, Pool_common.Message.error) result Lwt.t
end

module PhoneVerification : sig
  val message_params : Pool_common.VerificationCode.t -> (string * string) list

  val create_text_message
    :  Pool_database.Label.t
    -> Pool_common.Language.t
    -> Pool_tenant.t
    -> Pool_user.CellPhone.t
    -> Pool_common.VerificationCode.t
    -> (Text_message.t, Pool_common.Message.error) result Lwt.t
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
    -> (Contact.t -> (Sihl_email.t, Pool_common.Message.error) result) Lwt.t
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
        -> (Sihl_email.t, Pool_common.Message.error) result)
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
        -> (Text_message.t, Pool_common.Message.error) result)
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
    -> Sihl_email.t Lwt.t

  val prepare_emails
    :  Pool_database.Label.t
    -> Pool_tenant.t
    -> Pool_common.Language.t list
    -> Experiment.t
    -> Session.t
    -> (Assignment.t -> (Sihl_email.t, Pool_common.Message.error) result) Lwt.t

  val prepare_text_messages
    :  Pool_database.Label.t
    -> Pool_tenant.t
    -> Pool_common.Language.t list
    -> Experiment.t
    -> Session.t
    -> (Assignment.t
        -> Pool_user.CellPhone.t
        -> (Text_message.t, Pool_common.Message.error) result)
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
        -> (Sihl_email.t, Pool_common.Message.error) result)
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
    -> Sihl_email.t Lwt.t
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
    -> ([< `Admin of Admin.t | `Contact of Contact.t ]
        -> string
        -> Sihl_email.t)
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
    -> (Sihl_email.t, Pool_common.Message.error) result Lwt.t
end
