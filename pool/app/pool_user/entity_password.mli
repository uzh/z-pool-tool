module Policy : sig
  type rule =
    | MinLength of int
    | MustContainCapitalLetter
    | MustContainNumber
    | MustContainSpecialChar of char list

  type t

  val default_special_char_set : char list
  val default : t
end

module Confirmation : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> 'a -> unit
  val show : t -> string
  val create : string -> t

  val schema
    :  ?field:Pool_message.Field.t
    -> unit
    -> (Pool_message.Error.t, t) Pool_conformist.Field.t
end

module Plain : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> 'a -> unit
  val show : t -> string
  val create : string -> t

  val schema
    :  ?field:Pool_message.Field.t
    -> ?validation:(t -> (t, Pool_message.Error.t) result)
    -> unit
    -> (Pool_message.Error.t, t) Pool_conformist.Field.t

  val validate : t -> (t, Pool_message.Error.t) result
end

val to_confirmed : Plain.t -> Confirmation.t

val validate_confirmation
  :  Plain.t
  -> Confirmation.t
  -> (unit, Pool_message.Error.t) result

type t

val equal : t -> t -> bool
val pp : Format.formatter -> 'a -> unit
val show : t -> string
val of_hash : string -> t
val value : t -> string

val schema
  :  ?field:Pool_message.Field.t
  -> unit
  -> (Pool_message.Error.t, t) Pool_conformist.Field.t

val create : Plain.t -> Confirmation.t -> (t, Pool_message.Error.t) result

val update
  :  t
  -> old_password:Plain.t
  -> new_password:Plain.t
  -> new_password_confirmation:Confirmation.t
  -> (t, Pool_message.Error.t) result

val validate : t -> Plain.t -> bool

val validate_res
  :  ?field:Pool_message.Field.t
  -> t
  -> Plain.t
  -> (unit, Pool_message.Error.t) result
