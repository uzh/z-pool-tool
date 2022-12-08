module CustomTemplate : sig
  module Subject : sig
    type t =
      | I18n of I18n.t
      | String of string

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val value : t -> string
    val i18n : I18n.t -> t
    val string : string -> t
  end

  module Content : sig
    type t =
      | I18n of I18n.t
      | String of string

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val value : t -> string
    val i18n : I18n.t -> t
    val string : string -> t
  end

  type t =
    { subject : Subject.t
    ; content : Content.t
    }

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

module Token : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> t
  val value : t -> string
end

module VerifiedAt : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val value : t -> Ptime.t
  val create : Ptime.t -> t
  val create_now : unit -> t
end

type email_unverified =
  { address : Pool_user.EmailAddress.t
  ; user : Sihl_user.t
  ; token : Token.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }

type email_verified =
  { address : Pool_user.EmailAddress.t
  ; user : Sihl_user.t
  ; verified_at : VerifiedAt.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }

type unverified
type verified

val equal_email_unverified : email_unverified -> email_unverified -> bool
val equal_email_verified : email_verified -> email_verified -> bool
val pp_email_unverified : Format.formatter -> email_unverified -> unit
val pp_email_verified : Format.formatter -> email_verified -> unit
val show_email_unverified : email_unverified -> string
val show_email_verified : email_verified -> string

type _ t =
  | Unverified : email_unverified -> unverified t
  | Verified : email_verified -> verified t

val equal : 'email t -> 'email t -> bool
val pp : Format.formatter -> 'email t -> unit
val show : 'state t -> string
val token : unverified t -> string
val verify : unverified t -> verified t
val address : 'email t -> Pool_user.EmailAddress.t
val user_id : 'email t -> Pool_common.Id.t

val find_unverified_by_user
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> (unverified t, Pool_common.Message.error) result Lwt.t

val find_verified_by_user
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> (verified t, Pool_common.Message.error) result Lwt.t

val find_unverified_by_address
  :  Pool_database.Label.t
  -> Pool_user.EmailAddress.t
  -> (unverified t, Pool_common.Message.error) result Lwt.t

val delete_unverified_by_user
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> unit Lwt.t

module TemplateLabel : sig
  type t =
    | Boilerplate
    | EmailVerification
    | Invitation
    | PasswordChange
    | PasswordReset
    | SignUpVerification
    | SessionCancellation

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val read : string -> t
end

type text_component = (string, string) CCPair.t
type default

val default_values_root : default
val default_values_tenant : default

type verification_event =
  | Created of
      Pool_user.EmailAddress.t
      * Pool_common.Id.t
      * Pool_user.Firstname.t
      * Pool_user.Lastname.t
      * Pool_common.Language.t
  | Updated of Pool_user.EmailAddress.t * Sihl_user.t * Pool_common.Language.t
  | EmailVerified of unverified t

val handle_verification_event
  :  Pool_database.Label.t
  -> verification_event
  -> unit Lwt.t

val equal_verification_event : verification_event -> verification_event -> bool
val pp_verification_event : Format.formatter -> verification_event -> unit

type confirmation_email =
  { subject : I18n.Content.t
  ; text : I18n.Content.t
  ; language : Pool_common.Language.t
  ; session_text : string
  }

type event =
  | Sent of Sihl_email.t
  | BulkSent of Sihl_email.t list
  | ResetPassword of Sihl_user.t * Pool_common.Language.t
  | ChangedPassword of Sihl_user.t * Pool_common.Language.t
  | AssignmentConfirmationSent of Sihl_user.t * confirmation_email
  | InvitationSent of Sihl_user.t * text_component list * CustomTemplate.t
  | InvitationBulkSent of
      (Sihl_user.t * text_component list * CustomTemplate.t) list
  | DefaultRestored of default

val handle_event : Pool_database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val verification_event_name : verification_event -> string

module Helper : sig
  val prepare_email
    :  Pool_database.Label.t
    -> Pool_common.Language.t
    -> TemplateLabel.t
    -> string
    -> string
    -> (string * string) list
    -> Sihl_email.t Lwt.t

  val prepare_boilerplate_email
    :  CustomTemplate.t
    -> string
    -> (string * string) list
    -> Sihl_email.t

  module ConfirmationEmail : sig
    val create
      :  Pool_database.Label.t
      -> Pool_common.Language.t
      -> unverified t
      -> Pool_user.Firstname.t option
      -> Pool_user.Lastname.t option
      -> TemplateLabel.t
      -> Sihl_email.t Lwt.t
  end
end
