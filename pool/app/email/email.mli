module PoolError = Pool_common.Message
module User = Common_user

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
  { address : User.EmailAddress.t
  ; token : Token.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }

type email_verified =
  { address : User.EmailAddress.t
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
val create : User.EmailAddress.t -> Token.t -> unverified t
val verify : unverified t -> verified t
val address : 'email t -> User.EmailAddress.t

val find_unverified
  :  Pool_common.Database.Label.t
  -> User.EmailAddress.t
  -> (unverified t, PoolError.error) result Lwt.t

val find_verified
  :  Pool_common.Database.Label.t
  -> User.EmailAddress.t
  -> (verified t, PoolError.error) result Lwt.t

module PasswordReset : sig
  val create
    :  Pool_common.Database.Label.t
    -> user:Sihl_user.t
    -> (Sihl_email.t, PoolError.error) result Lwt.t
end

module PasswordChange : sig
  val create
    :  Pool_common.Database.Label.t
    -> verified t
    -> User.Firstname.t
    -> User.Lastname.t
    -> Sihl_email.t Lwt.t
end

module ConfirmationEmail : sig
  val create
    :  Pool_common.Database.Label.t
    -> unverified t
    -> User.Firstname.t
    -> User.Lastname.t
    -> Sihl_email.t Lwt.t
end

type event =
  | Created of User.EmailAddress.t * User.Firstname.t * User.Lastname.t
  | UpdatedUnverified of
      unverified t * (User.EmailAddress.t * User.Firstname.t * User.Lastname.t)
  | UpdatedVerified of
      verified t * (User.EmailAddress.t * User.Firstname.t * User.Lastname.t)
  | Verified of unverified t

val handle_event : Pool_common.Database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
