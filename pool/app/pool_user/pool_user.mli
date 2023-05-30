module PasswordConfirmed : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> t
  val to_sihl : t -> string

  val schema
    :  ?field:Pool_common.Message.Field.t
    -> unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

module Password : sig
  module Policy : sig
    type rule =
      | MinLength of int
      | MustContainCapitalLetter
      | MustContainNumber
      | MustContainSpecialChar of char list

    type t

    val default_special_char_set : char list
    val default_policy : t
  end

  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val to_sihl : t -> string
  val create : string -> (t, Pool_common.Message.error) result
  val create_unvalidated : string -> (t, Pool_common.Message.error) result

  val schema
    :  ?field:Pool_common.Message.Field.t
    -> (string -> (t, Pool_common.Message.error) result)
    -> unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t

  val validate_current_password
    :  ?field:Pool_common.Message.Field.t
    -> Sihl_user.t
    -> t
    -> (unit, Pool_common.Message.error) result

  val validate_password_confirmation
    :  t
    -> PasswordConfirmed.t
    -> (unit, Pool_common.Message.error) result
end

module Firstname : sig
  include Pool_common.Model.StringSig

  val of_string : string -> t
end

module Lastname : sig
  include Pool_common.Model.StringSig

  val of_string : string -> t
end

module Paused : sig
  include Pool_common.Model.BooleanSig
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
  val create_now : unit -> t
  val value : t -> Ptime.t
end

module Verified : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : Ptime.t -> t
  val create_now : unit -> t
  val value : t -> Ptime.t
end

module PhoneNumber : sig
  type t

  val create : string -> (t, Pool_common.Message.error) result
  val of_string : string -> t
  val value : t -> string
  val equal : t -> t -> bool
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val pp : Format.formatter -> t -> unit
end

module UnverifiedPhoneNumber : sig
  type t =
    { phone_number : PhoneNumber.t
    ; created_at : Pool_common.CreatedAt.t
    }

  type full =
    { phone_number : PhoneNumber.t
    ; token : Pool_common.Token.t
    ; created_at : Pool_common.CreatedAt.t
    }
end

module EmailAddress : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val validate_characters : t -> (t, Pool_common.Message.error) result

  val validate
    :  Settings.EmailSuffix.t list option
    -> t
    -> (unit, Pool_common.Message.error) result

  val value : t -> string
  val create : string -> (t, Pool_common.Message.error) result
  val of_string : string -> t

  val schema
    :  unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

module EmailVerified : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : Ptime.t -> t
  val create_now : unit -> t
  val value : t -> Ptime.t
end

module Repo : sig
  module Paused : sig
    val t : bool Caqti_type.t
  end

  module Disabled : sig
    val t : bool Caqti_type.t
  end

  module TermsAccepted : sig
    val t : Ptime.t Caqti_type.t
  end

  module Verified : sig
    val t : Ptime.t Caqti_type.t
  end

  module EmailVerified : sig
    val t : Ptime.t Caqti_type.t
  end

  module PhoneNumber : sig
    val t : string Caqti_type.t
  end

  module UnverifiedPhoneNumber : sig
    val t : UnverifiedPhoneNumber.t Caqti_type.t
    val full : UnverifiedPhoneNumber.full Caqti_type.t
  end

  module EmailAddress : sig
    val t : string Caqti_type.t
  end

  val user_caqti : Sihl_user.t Caqti_type.t
end

val user_firstname : Sihl_user.t -> Firstname.t
val user_lastname : Sihl_user.t -> Lastname.t
val user_fullname : Sihl_user.t -> string
val user_email_address : Sihl_user.t -> EmailAddress.t
