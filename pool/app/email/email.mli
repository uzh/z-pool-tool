module PoolError = Pool_common.Message
module User = Common_user

type email_unverified =
  { address : User.Email.Address.t
  ; token : User.Email.Token.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }

type email_verified =
  { address : User.Email.Address.t
  ; verified_at : User.Email.VerifiedAt.t
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
val create : User.Email.Address.t -> User.Email.Token.t -> unverified t
val verify : unverified t -> verified t
val address : 'email t -> User.Email.Address.t

val find_unverified
  :  Pool_common.Database.Label.t
  -> User.Email.Address.t
  -> (unverified t, PoolError.error) result Lwt.t

val find_verified
  :  Pool_common.Database.Label.t
  -> User.Email.Address.t
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
  | Created of User.Email.Address.t * User.Firstname.t * User.Lastname.t
  | UpdatedUnverified of
      unverified t * (User.Email.Address.t * User.Firstname.t * User.Lastname.t)
  | UpdatedVerified of
      verified t * (User.Email.Address.t * User.Firstname.t * User.Lastname.t)
  | Verified of unverified t

val handle_event : Pool_common.Database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
