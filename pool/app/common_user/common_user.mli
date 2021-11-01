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
  val schema : unit -> ('a, t) Conformist.Field.t
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
  val value : t -> string
  val schema : unit -> ('a, t) Conformist.Field.t
end

module Lastname : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> (t, string) result
  val value : t -> string
  val schema : unit -> ('a, t) Conformist.Field.t
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
  val create : Ptime.t option -> t
  val create_now : t
  val value : t -> Ptime.t option
end

module Verified : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : Ptime.t option -> t
  val create_now : t
  val value : t -> Ptime.t option
end

module Email : sig
  module Token : sig
    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val create : string -> t
    val value : t -> string
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

    val value : t -> string
    val create : string -> (t, string) result
    val schema : unit -> ('a, t) Conformist.Field.t
  end

  module VerifiedAt : sig
    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  type email_unverified =
    { address : Address.t
    ; token : Token.t
    }

  type email_verified =
    { address : Address.t
    ; verified_at : VerifiedAt.t
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
  val create : Address.t -> Token.t -> unverified t
  val verify : unverified t -> verified t
  val address : 'email t -> string
end

module Repo : sig
  module Paused : sig
    val t : bool Caqti_type.t
  end

  module Disabled : sig
    val t : bool Caqti_type.t
  end

  module TermsAccepted : sig
    val t : Ptime.t option Caqti_type.t
  end

  module Verified : sig
    val t : Ptime.t option Caqti_type.t
  end

  module Email : sig
    val unverified_t : Entity_email.unverified Entity_email.t Caqti_type.t
    val verified_t : Entity_email.verified Entity_email.t Caqti_type.t
    val insert : Pool_common.Database.Label.t -> 'a -> 'b
    val update : Pool_common.Database.Label.t -> 'a -> 'b
  end

  val user_caqti : Sihl_user.t Caqti_type.t
end

module Event : sig
  module Email : sig
    type event =
      | Created of Email.Address.t * Firstname.t * Lastname.t
      | UpdatedUnverified of
          Email.unverified Email.t
          * (Email.Address.t * Firstname.t * Lastname.t)
      | UpdatedVerified of
          Email.verified Email.t * (Email.Address.t * Firstname.t * Lastname.t)
      | Verified of Email.unverified Email.t

    val handle_event : Pool_common.Database.Label.t -> event -> unit Lwt.t
    val equal_event : event -> event -> bool
    val pp_event : Format.formatter -> event -> unit

    module PasswordReset : sig
      val create
        :  Pool_common.Database.Label.t
        -> string
        -> user:Sihl_user.t
        -> (Sihl_email.t, string) result Lwt.t
    end
  end
end
