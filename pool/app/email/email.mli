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
    | EmailVerification
    | PasswordChange
    | PasswordReset
    | SignUpVerification

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val read : string -> t
end

type default

val default_values_root : default
val default_values_tenant : default

type event =
  | Created of
      Pool_user.EmailAddress.t
      * Pool_common.Id.t
      * Pool_user.Firstname.t
      * Pool_user.Lastname.t
      * Pool_common.Language.t
  | Updated of Pool_user.EmailAddress.t * Sihl_user.t * Pool_common.Language.t
  | EmailVerified of unverified t
  | DefaultRestored of default

val handle_event : Pool_database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit

module Helper : sig
  module PasswordReset : sig
    val create
      :  Pool_database.Label.t
      -> Pool_common.Language.t
      -> user:Sihl_user.t
      -> (Sihl_email.t, Pool_common.Message.error) result Lwt.t
  end

  module PasswordChange : sig
    val create
      :  Pool_database.Label.t
      -> Pool_common.Language.t
      -> verified t
      -> Pool_user.Firstname.t
      -> Pool_user.Lastname.t
      -> Sihl_email.t Lwt.t
  end

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

  module Invitation : sig
    val create
      :  Pool_database.Label.t
      -> Pool_user.EmailAddress.t
      -> string
      -> Sihl_email.t Lwt.t
  end
end
