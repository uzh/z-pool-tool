module PoolError = Pool_common.Message
module Database = Pool_database
module User = Pool_user

module Token = struct
  type t = string [@@deriving eq, show]

  let create m = m
  let value m = m
end

module VerifiedAt = struct
  type t = Ptime.t [@@deriving eq, show]

  let value m = m
  let create m = m
  let create_now = Ptime_clock.now
end

type email_unverified =
  { address : User.EmailAddress.t
  ; token : Token.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show]

type email_verified =
  { address : User.EmailAddress.t
  ; verified_at : VerifiedAt.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show]

(* TODO hide private constructors if possible *)
(* Don't use these private constructors *)
(* They are needed so the typechecker understands they are disjoint *)
type unverified = private XUnverifiedP
type verified = private XVerifiedP

type _ t =
  | Unverified : email_unverified -> unverified t
  | Verified : email_verified -> verified t

(* Carries type information, is a type "witness" *)
type _ carrier =
  | UnverifiedC : unverified carrier
  | VerifiedC : verified carrier

let equal : type state. state t -> state t -> bool =
 fun m k ->
  match m, k with
  | Unverified one, Unverified two -> equal_email_unverified one two
  | Verified one, Verified two -> equal_email_verified one two
;;

let pp : type state. Format.formatter -> state t -> unit =
 fun formatter email ->
  match email with
  | Unverified m -> pp_email_unverified formatter m
  | Verified m -> pp_email_verified formatter m
;;

let show : type state. state t -> string = function
  | Unverified { address; _ } | Verified { address; _ } ->
    User.EmailAddress.show address
;;

let address : type state. state t -> User.EmailAddress.t = function
  | Unverified { address; _ } | Verified { address; _ } -> address
;;

let token (Unverified email) = Token.value email.token

let create address token =
  Unverified
    { address
    ; token
    ; created_at = Ptime_clock.now ()
    ; updated_at = Ptime_clock.now ()
    }
;;

let verify (Unverified email) =
  Verified
    { address = email.address
    ; verified_at = VerifiedAt.create_now ()
    ; created_at = email.created_at
    ; updated_at = Ptime_clock.now ()
    }
;;
