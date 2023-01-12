module Id : sig
  include module type of Pool_common.Id
end

module Label : sig
  type t =
    | AssignmentConfirmation
    | EmailVerification
    | ExperimentInvitation
    | PasswordChange
    | PasswordReset
    | ProfileUpdateTrigger
    | SignUpVerification
    | SessionCancellation
    | SessionReminder
    | SessionReschedule

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val read : string -> t
  val of_string : string -> (t, Pool_common.Message.error) result
  val to_human : t -> string
  val human_url : t -> string
  val prefixed_human_url : t -> string
end

module EmailSubject : sig
  include Pool_common.Model.StringSig
end

module EmailText : sig
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
  ; sms_text : SmsText.t
  }

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val show : t -> string

type update =
  { email_subject : EmailSubject.t
  ; email_text : EmailText.t
  ; sms_text : SmsText.t
  }

type event =
  | Created of t
  | DefaultRestored of t list
  | Updated of t * update

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
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
      :  ?ctx:Guardian__Persistence.context
      -> t
      -> ( [> `MessageTemplate ] Guard.AuthorizableTarget.t
         , Pool_common.Message.error )
         Lwt_result.t

    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end
end

val create_public_url_with_params
  :  Pool_tenant.Url.t
  -> string
  -> (Pool_common.Message.Field.t * string) list
  -> string

module AssignmentConfirmation : sig
  val email_params
    :  Pool_common.Language.t
    -> Session.t
    -> Contact.t
    -> (string * string) list

  val create
    :  Pool_database.Label.t
    -> Pool_common.Language.t
    -> Pool_tenant.t
    -> Session.t
    -> Contact.t
    -> (Sihl_email.t, Pool_common.Message.error) result Lwt.t

  val create_from_public_session
    :  Pool_database.Label.t
    -> Pool_common.Language.t
    -> Pool_tenant.t
    -> Session.Public.t
    -> Contact.t
    -> (Sihl_email.t, Pool_common.Message.error) result Lwt.t
end

module EmailVerification : sig
  val email_params : string -> Contact.t -> (string * string) list

  val create
    :  Pool_database.Label.t
    -> Pool_common.Language.t
    -> layout
    -> Contact.t
    -> Pool_user.EmailAddress.t
    -> Email.Token.t
    -> (Sihl_email.t, Pool_common.Message.error) result Lwt.t
end

module ExperimentInvitation : sig
  val email_params
    :  Experiment.t
    -> Pool_tenant.Url.t
    -> Contact.t
    -> (string * string) list

  val create
    :  Pool_tenant.t
    -> Experiment.t
    -> Contact.t
    -> (Sihl_email.t, Pool_common.Message.error) result Lwt.t

  val prepare
    :  Pool_tenant.t
    -> ( Experiment.t
         -> Contact.t
         -> (Sihl_email.t, Pool_common.Message.error) result
       , Pool_common.Message.error )
       result
       Lwt.t
end

module PasswordChange : sig
  val email_params : Sihl_user.t -> (string * string) list

  val create
    :  Pool_database.Label.t
    -> Pool_common.Language.t
    -> Pool_tenant.t
    -> Sihl_user.t
    -> (Sihl_email.t, Pool_common.Message.error) result Lwt.t
end

module PasswordReset : sig
  val email_params : string -> Sihl_user.t -> (string * string) list

  val create
    :  Pool_database.Label.t
    -> Pool_common.Language.t
    -> layout
    -> Sihl_user.t
    -> (Sihl_email.t, Pool_common.Message.error) result Lwt.t
end

module ProfileUpdateTrigger : sig
  val email_params : Pool_tenant.Url.t -> Contact.t -> (string * string) list

  val prepare
    :  Pool_database.Label.t
    -> Pool_tenant.t
    -> ( Contact.t -> (Sihl_email.t, Pool_common.Message.error) result
       , Pool_common.Message.error )
       result
       Lwt.t
end

module SessionCancellation : sig
  val email_params
    :  Pool_common.Language.t
    -> Session.t
    -> Session.CancellationReason.t
    -> Contact.t
    -> (string * string) list

  val prepare
    :  Pool_database.Label.t
    -> Pool_tenant.t
    -> Pool_common.Language.t list
    -> Session.t
    -> ( Session.CancellationReason.t
         -> Contact.t
         -> (Sihl_email.t, Pool_common.Message.error) result
       , Pool_common.Message.error )
       result
       Lwt.t
end

module SessionReminder : sig
  val email_params
    :  Pool_common.Language.t
    -> Experiment.t
    -> Session.t
    -> Contact.t
    -> (string * string) list

  val create
    :  Pool_database.Label.t
    -> Pool_tenant.t
    -> Pool_common.Language.t list
    -> Experiment.t
    -> Session.t
    -> Contact.t
    -> (Sihl_email.t, Pool_common.Message.error) result Lwt.t
end

module SessionReschedule : sig
  val email_params
    :  Pool_common.Language.t
    -> Session.t
    -> Session.Start.t
    -> Session.Duration.t
    -> Contact.t
    -> (string * string) list

  val prepare
    :  Pool_database.Label.t
    -> Pool_tenant.t
    -> Pool_common.Language.t list
    -> Session.t
    -> ( Contact.t
         -> Session.Start.t
         -> Session.Duration.t
         -> (Sihl_email.t, Pool_common.Message.error) result
       , Pool_common.Message.error )
       result
       Lwt.t
end

module SignUpVerification : sig
  val email_params
    :  string
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
    -> (Sihl_email.t, Pool_common.Message.error) result Lwt.t
end
