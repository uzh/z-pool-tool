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

type event = DefaultRestored of t list

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val handle_event : Pool_database.Label.t -> event -> unit Lwt.t
val default_values_tenant : t list
val default_values_root : t list

type layout =
  | Tenant of Pool_tenant.t
  | Root

module AssignmentConfirmation : sig
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
  val create
    :  Pool_tenant.t
    -> Experiment.t
    -> Contact.t
    -> (Sihl_email.t, Pool_common.Message.error) result Lwt.t

  val prepare_template_list
    :  Pool_tenant.t
    -> ( Experiment.t
         -> Contact.t
         -> (Sihl_email.t, Pool_common.Message.error) result
       , Pool_common.Message.error )
       result
       Lwt.t
end

module PasswordChange : sig
  val create
    :  Pool_database.Label.t
    -> Pool_common.Language.t
    -> Pool_tenant.t
    -> Sihl_user.t
    -> (Sihl_email.t, Pool_common.Message.error) result Lwt.t
end

module ProfileUpdateTrigger : sig
  val prepare_template_list
    :  Pool_database.Label.t
    -> Pool_tenant.t
    -> ( Contact.t -> (Sihl_email.t, Pool_common.Message.error) result
       , Pool_common.Message.error )
       result
       Lwt.t
end

module PasswordReset : sig
  val create
    :  Pool_database.Label.t
    -> Pool_common.Language.t
    -> layout
    -> Sihl_user.t
    -> (Sihl_email.t, Pool_common.Message.error) result Lwt.t
end

module SessionCancellation : sig
  val prepare_template_list
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
  val prepare_template_list
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
