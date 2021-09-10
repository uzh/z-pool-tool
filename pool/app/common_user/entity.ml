module Email = Entity_email

module Id : sig
  type t

  val create : unit -> t
end = struct
  type t = string

  let create () = Uuidm.create `V4 |> Uuidm.to_string
end

module Password : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string

  val create
    :  ?password_policy:(string -> (unit, string) result)
    -> string
    -> unit
    -> (t, string) result

  val to_sihl : t -> string
end = struct
  type t = string [@@deriving eq]

  let default_password_policy p =
    if String.length p < 8 then Error "password_policy_text" else Ok ()
  ;;

  let create ?(password_policy = default_password_policy) password () =
    let ( let* ) = Result.bind in
    let* () = password_policy password in
    Ok password
  ;;

  let to_sihl m = m
  let show m = CCString.repeat "*" @@ CCString.length m

  let pp (formatter : Format.formatter) (m : t) : unit =
    Format.fprintf formatter "%s" m
  ;;
end

module PasswordConfirmed : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> t
  val to_sihl : t -> string
end = struct
  type t = string [@@deriving eq]

  let create m = m
  let to_sihl m = m
  let show m = CCString.repeat "*" @@ CCString.length m

  let pp (formatter : Format.formatter) (m : t) : unit =
    Format.fprintf formatter "%s" m
  ;;
end

module Firstname : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> (t, string) result
end = struct
  type t = string [@@deriving eq, show]

  let create m =
    if String.length m <= 0 then Error "Invalid firstname" else Ok m
  ;;
end

module Lastname : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> (t, string) result
end = struct
  type t = string [@@deriving eq, show]

  let create m = if String.length m <= 0 then Error "Invalid lastname" else Ok m
end

module Paused : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val value : t -> bool
  val create : bool -> t
end = struct
  type t = bool [@@deriving eq, show]

  let create m = m
  let value m = m
end

module Disabled : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val value : t -> bool
  val create : bool -> t
end = struct
  type t = bool [@@deriving eq, show]

  let create m = m
  let value m = m
end

module TermsAccepted : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : Ptime.t -> t
  val create_now : t
  val value : t -> Ptime.t
end = struct
  type t = Ptime.t [@@deriving eq, show]

  let create m = m
  let create_now = Ptime_clock.now ()
  let value m = m
end

module Verified : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : Ptime.t -> t
  val create_now : t
  val value : t -> Ptime.t
end = struct
  type t = Ptime.t [@@deriving eq, show]

  let create m = m
  let create_now = Ptime_clock.now ()
  let value m = m
end
