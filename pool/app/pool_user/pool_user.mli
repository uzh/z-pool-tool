include module type of Sihl.Contract.User
module Persistence : Service_user_sig.Sig
module PasswordReset : Sihl.Contract.Password_reset.Sig

module PasswordConfirmed : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> t
  val to_sihl : t -> string

  val schema
    :  ?field:Pool_message.Field.t
    -> unit
    -> (Pool_message.Error.t, t) Pool_conformist.Field.t
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
  val create : string -> (t, Pool_message.Error.t) result
  val create_unvalidated : string -> (t, Pool_message.Error.t) result

  val schema
    :  ?field:Pool_message.Field.t
    -> (string -> (t, Pool_message.Error.t) result)
    -> unit
    -> (Pool_message.Error.t, t) Pool_conformist.Field.t

  val validate_current_password
    :  ?field:Pool_message.Field.t
    -> Sihl_user.t
    -> t
    -> (unit, Pool_message.Error.t) result

  val validate_password_confirmation
    :  t
    -> PasswordConfirmed.t
    -> (unit, Pool_message.Error.t) result
end

module Firstname : sig
  include Pool_model.Base.StringSig

  val of_string : string -> t
end

module Lastname : sig
  include Pool_model.Base.StringSig

  val of_string : string -> t
end

module Paused : sig
  include Pool_model.Base.BooleanSig
end

module Disabled : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val value : t -> bool
  val create : bool -> t
  val compare : t -> t -> int
end

module TermsAccepted : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : Ptime.t -> t
  val create_now : unit -> t
  val value : t -> Ptime.t
  val compare : t -> t -> int
end

module Verified : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : Ptime.t -> t
  val create_now : unit -> t
  val value : t -> Ptime.t
  val compare : t -> t -> int
end

module CellPhone : sig
  type t

  val create : string -> (t, Pool_message.Error.t) result
  val of_string : string -> t
  val value : t -> string
  val equal : t -> t -> bool
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val pp : Format.formatter -> t -> unit
  val compare : t -> t -> int

  val schema_test_cell_phone
    :  unit
    -> (Pool_message.Error.t, t) Pool_conformist.Field.t
end

module ImportPending : sig
  include Pool_model.Base.BooleanSig
end

module UnverifiedCellPhone : sig
  type t =
    { cell_phone : CellPhone.t
    ; created_at : Pool_common.CreatedAt.t
    }

  type full =
    { cell_phone : CellPhone.t
    ; verification_code : Pool_common.VerificationCode.t
    ; created_at : Pool_common.CreatedAt.t
    }
end

module EmailAddress : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val validate_characters : t -> (t, Pool_message.Error.t) result

  val validate
    :  Settings.EmailSuffix.t list option
    -> t
    -> (unit, Pool_message.Error.t) result

  val value : t -> string
  val create : string -> (t, Pool_message.Error.t) result
  val of_string : string -> t
  val schema : unit -> (Pool_message.Error.t, t) Pool_conformist.Field.t
end

module EmailVerified : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : Ptime.t -> t
  val create_now : unit -> t
  val value : t -> Ptime.t
  val compare : t -> t -> int
end

module Repo : sig
  module Paused : sig
    val t : Paused.t Caqti_type.t
  end

  module Disabled : sig
    val t : Disabled.t Caqti_type.t
  end

  module TermsAccepted : sig
    val t : TermsAccepted.t Caqti_type.t
  end

  module Verified : sig
    val t : Verified.t Caqti_type.t
  end

  module EmailVerified : sig
    val t : EmailVerified.t Caqti_type.t
  end

  module CellPhone : sig
    val t : CellPhone.t Caqti_type.t
  end

  module ImportPending : sig
    val t : ImportPending.t Caqti_type.t
  end

  module UnverifiedCellPhone : sig
    val t : UnverifiedCellPhone.t Caqti_type.t
    val full : UnverifiedCellPhone.full Caqti_type.t
  end

  module EmailAddress : sig
    val t : EmailAddress.t Caqti_type.t
  end

  val user_caqti : Sihl_user.t Caqti_type.t
  val sql_select_columns : string list
end

val user_firstname : Sihl_user.t -> Firstname.t
val user_lastname : Sihl_user.t -> Lastname.t
val user_fullname : Sihl_user.t -> string
val user_lastname_firstname : Sihl_user.t -> string
val user_email_address : Sihl_user.t -> EmailAddress.t
val searchable_by : Query.Column.t list
val sortable_by : Query.Column.t list
val default_sort : Query.Sort.t
val default_query : Query.t
val column_first_name : Query.Column.t
val column_last_name : Query.Column.t
val column_name : Query.Column.t
val column_email : Query.Column.t
val column_inactive : Query.Column.t

val find_active_user_by_email_opt
  :  Database.Label.t
  -> EmailAddress.t
  -> t option Lwt.t

val create_session
  :  Database.Label.t
  -> EmailAddress.t
  -> password:string
  -> (t, [ `Does_not_exist | `Incorrect_password ]) Lwt_result.t

module FailedLoginAttempt : sig
  module Id : sig
    include Pool_model.Base.IdSig
  end

  module Counter : sig
    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val create : int -> t
    val init : t
    val value : t -> int
    val increment : t -> t
  end

  module BlockedUntil : sig
    type t

    val equal : t -> t -> bool
    val show : t -> string
    val pp : Format.formatter -> t -> unit
    val value : t -> Ptime.t
    val create : Ptime.t -> (t, Pool_message.Error.t) result
  end

  type t =
    { id : Id.t
    ; email : EmailAddress.t
    ; counter : Counter.t
    ; blocked_until : BlockedUntil.t option
    }

  val create
    :  ?id:Id.t
    -> EmailAddress.t
    -> Counter.t
    -> BlockedUntil.t option
    -> t

  module Repo : sig
    val find_opt : Database.Label.t -> EmailAddress.t -> t option Lwt.t
    val insert : Database.Label.t -> t -> unit Lwt.t
    val delete : Database.Label.t -> t -> unit Lwt.t
  end
end
