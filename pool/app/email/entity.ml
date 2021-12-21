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
  ; user : Sihl_user.t
        [@equal fun m k -> CCString.equal m.Sihl_user.id k.Sihl_user.id]
  ; token : Token.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show]

type email_verified =
  { address : User.EmailAddress.t
  ; user : Sihl_user.t
        [@equal fun m k -> CCString.equal m.Sihl_user.id k.Sihl_user.id]
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

let user_id : type state. state t -> Pool_common.Id.t = function
  | Unverified { user; _ } | Verified { user; _ } ->
    user.Sihl.Contract.User.id |> Pool_common.Id.of_string
;;

let address : type state. state t -> User.EmailAddress.t = function
  | Unverified { address; _ } | Verified { address; _ } -> address
;;

let token (Unverified email) = Token.value email.token

let create pool address user_id token =
  let open Lwt.Infix in
  let ctx = Pool_tenant.to_ctx pool in
  user_id
  |> Pool_common.Id.value
  |> Service.User.find_opt ~ctx
  >|= fun user ->
  match user with
  | Some user ->
    Unverified
      { address
      ; user
      ; token
      ; created_at = Ptime_clock.now ()
      ; updated_at = Ptime_clock.now ()
      }
  | None -> failwith (PoolError.(NotFound User) |> PoolError.show_error)
;;

let verify (Unverified email) =
  Verified
    { address = email.address
    ; user = email.user
    ; verified_at = VerifiedAt.create_now ()
    ; created_at = email.created_at
    ; updated_at = Ptime_clock.now ()
    }
;;
