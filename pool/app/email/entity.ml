open CCFun.Infix
module SmtpAuth = Entity_smtp
module User = Pool_user

module Sihl_email = struct
  include Sihl_email

  let equal (e1 : t) (e2 : t) =
    let open CCString in
    equal e1.sender e2.sender
    && equal e1.recipient e2.recipient
    && equal e1.subject e2.subject
    && equal e1.text e2.text
  ;;

  let yojson_of_t = Sihl.Contract.Email.to_yojson

  let t_of_yojson =
    of_yojson
    %> function
    | Some email -> email
    | None -> Yojson.json_error "Invalid serialized email string received"
  ;;
end

module Token = struct
  include Pool_model.Base.String

  let create m = m
end

module VerifiedAt = Pool_model.Base.Ptime

type email_unverified =
  { address : User.EmailAddress.t
  ; user : Pool_user.t
  ; token : Token.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show]

type email_verified =
  { address : User.EmailAddress.t
  ; user : Pool_user.t
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

let user_id : type state. state t -> Pool_user.Id.t = function
  | Unverified { user; _ } | Verified { user; _ } -> user.User.id
;;

let user_is_confirmed : type state. state t -> bool = function
  | Unverified { user; _ } | Verified { user; _ } ->
    user.Pool_user.confirmed |> Pool_user.Confirmed.value
;;

let address : type state. state t -> User.EmailAddress.t = function
  | Unverified { address; _ } | Verified { address; _ } -> address
;;

let token (Unverified email) = Token.value email.token

let create address user token =
  Unverified
    { address
    ; user
    ; token = Token.value token
    ; created_at = Pool_common.CreatedAt.create_now ()
    ; updated_at = Pool_common.UpdatedAt.create_now ()
    }
;;

let verify (Unverified email) =
  Verified
    { address = email.address
    ; user = email.user
    ; verified_at = VerifiedAt.create_now ()
    ; created_at = email.created_at
    ; updated_at = Pool_common.UpdatedAt.create_now ()
    }
;;

module Job = struct
  type t =
    { email : Sihl_email.t
    ; smtp_auth_id : SmtpAuth.Id.t option [@yojson.option]
    }
  [@@deriving eq, fields, show, yojson]

  let create ?smtp_auth_id email = { email; smtp_auth_id }

  let update ?new_email_address ?new_smtp_auth_id { email; smtp_auth_id } =
    let open CCOption in
    let email =
      let email_address = map Pool_user.EmailAddress.value new_email_address in
      Sihl.Contract.Email.
        { email with recipient = value ~default:email.recipient email_address }
    in
    let smtp_auth_id = new_smtp_auth_id <+> smtp_auth_id in
    { email; smtp_auth_id }
  ;;
end
