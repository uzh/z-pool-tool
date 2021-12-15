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
  { address : Common_user.EmailAddress.t
  ; token : Token.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }

type email_verified =
  { address : Common_user.EmailAddress.t
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
val create : Common_user.EmailAddress.t -> Token.t -> unverified t
val verify : unverified t -> verified t
val address : 'email t -> Common_user.EmailAddress.t

val find_unverified
  :  Database_pool.Label.t
  -> Common_user.EmailAddress.t
  -> (unverified t, Pool_common.Message.error) result Lwt.t

val find_verified
  :  Database_pool.Label.t
  -> Common_user.EmailAddress.t
  -> (verified t, Pool_common.Message.error) result Lwt.t

module PasswordReset : sig
  val create
    :  Database_pool.Label.t
    -> user:Sihl_user.t
    -> (Sihl_email.t, Pool_common.Message.error) result Lwt.t
end

module PasswordChange : sig
  val create
    :  Database_pool.Label.t
    -> verified t
    -> Common_user.Firstname.t
    -> Common_user.Lastname.t
    -> Sihl_email.t Lwt.t
end

module ConfirmationEmail : sig
  val create
    :  Database_pool.Label.t
    -> unverified t
    -> Common_user.Firstname.t
    -> Common_user.Lastname.t
    -> Sihl_email.t Lwt.t
end

type event =
  | Created of
      Common_user.EmailAddress.t
      * Common_user.Firstname.t
      * Common_user.Lastname.t
  | UpdatedUnverified of
      unverified t
      * (Common_user.EmailAddress.t
        * Common_user.Firstname.t
        * Common_user.Lastname.t)
  | UpdatedVerified of
      verified t
      * (Common_user.EmailAddress.t
        * Common_user.Firstname.t
        * Common_user.Lastname.t)
  | EmailVerified of unverified t

val handle_event : Database_pool.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
