module Password : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string

  val validate
    :  ?password_policy:(string -> (unit, string) result)
    -> t
    -> (unit, string) result

  val create : string -> (t, string) result
  val to_sihl : t -> string
end

module PasswordConfirmed : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> t
  val to_sihl : t -> string
end

module Firstname : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> (t, string) result
end

module Lastname : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> (t, string) result
end

module Paused : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val value : t -> bool
  val create : bool -> t
end

module Disabled : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val value : t -> bool
  val create : bool -> t
end

module TermsAccepted : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : Ptime.t -> t
  val create_now : t
  val value : t -> Ptime.t
end

module Verified : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : Ptime.t -> t
  val create_now : t
  val value : t -> Ptime.t
end

module Email : sig
  module Token : sig
    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module Address : sig
    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string

    val validate
      :  Settings.EmailSuffix.t list option
      -> t
      -> (unit, string) result

    val create : string -> (t, string) result
  end

  module VerifiedAt : sig
    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  type email_unverified
  type unverified
  type email_verified
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
  val create : Address.t -> Token.t -> (unverified t, string) result
  val verify : unverified t -> verified t
end

module Repo : sig
  module Paused : sig
    type t = Paused.t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val value : t -> bool
    val create : bool -> t
    val t : bool Caqti_type.t
  end

  module Disabled : sig
    type t = Disabled.t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val value : t -> bool
    val create : bool -> t
    val t : bool Caqti_type.t
  end

  module TermsAccepted : sig
    type t = TermsAccepted.t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val create : Ptime.t -> t
    val create_now : t
    val value : t -> Ptime.t
    val t : Ptime.t Caqti_type.t
  end

  module Verified : sig
    type t = Verified.t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val create : Ptime.t -> t
    val create_now : t
    val value : t -> Ptime.t
    val t : Ptime.t Caqti_type.t
  end

  module Email : sig
    val unverified_t : Entity_email.unverified Entity_email.t Caqti_type.t
    val verified_t : Entity_email.verified Entity_email.t Caqti_type.t
    val insert : 'a -> 'b
    val update : 'a -> 'b
  end
end

module Event : sig
  module Email : sig
    type event =
      | Created of Email.Address.t
      | UpdatedUnverified of
          Entity_email.unverified Entity_email.t * Email.Address.t
      | UpdatedVerified of
          Entity_email.verified Entity_email.t * Email.Address.t
      | Verified of Entity_email.unverified Entity_email.t

    val handle_event : event -> unit Lwt.t
    val equal_event : event -> event -> bool
    val pp_event : Format.formatter -> event -> unit
  end
end
