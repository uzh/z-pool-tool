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
  ; user_id : Pool_common.Id.t
  ; token : Token.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }

type email_verified =
  { address : Pool_user.EmailAddress.t
  ; user_id : Pool_common.Id.t
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

val create
  :  Pool_user.EmailAddress.t
  -> Pool_common.Id.t
  -> Token.t
  -> unverified t

val verify : unverified t -> verified t
val address : 'email t -> Pool_user.EmailAddress.t
val user_id : 'email t -> Pool_common.Id.t

val find_unverified
  :  Pool_database.Label.t
  -> Pool_user.EmailAddress.t
  -> (unverified t, Pool_common.Message.error) result Lwt.t

val find_verified
  :  Pool_database.Label.t
  -> Pool_user.EmailAddress.t
  -> (verified t, Pool_common.Message.error) result Lwt.t

type event =
  | Created of
      Pool_user.EmailAddress.t
      * Pool_common.Id.t
      * Pool_user.Firstname.t
      * Pool_user.Lastname.t
  | UpdatedUnverified of
      unverified t
      * (Pool_user.EmailAddress.t
        * Pool_user.Firstname.t
        * Pool_user.Lastname.t)
  | UpdatedVerified of
      verified t
      * (Pool_user.EmailAddress.t
        * Pool_user.Firstname.t
        * Pool_user.Lastname.t)
  | EmailVerified of unverified t

val handle_event : Pool_database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit

module Helper : sig
  module PasswordReset : sig
    val create
      :  Pool_database.Label.t
      -> user:Sihl_user.t
      -> (Sihl_email.t, Pool_common.Message.error) result Lwt.t
  end

  module PasswordChange : sig
    val create
      :  Pool_database.Label.t
      -> verified t
      -> Pool_user.Firstname.t
      -> Pool_user.Lastname.t
      -> Sihl_email.t Lwt.t
  end

  module ConfirmationEmail : sig
    val create
      :  Pool_database.Label.t
      -> unverified t
      -> Pool_user.Firstname.t
      -> Pool_user.Lastname.t
      -> Sihl_email.t Lwt.t
  end
end
