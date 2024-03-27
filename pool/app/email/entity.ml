module Database = Pool_database
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
end

module Token = struct
  type t = string [@@deriving eq, show]

  let create m = m
  let value m = m
end

module VerifiedAt = struct
  include Pool_model.Base.Ptime

  let create m = m
end

type email_unverified =
  { address : User.EmailAddress.t
  ; user : Sihl_user.t
  ; token : Token.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show]

type email_verified =
  { address : User.EmailAddress.t
  ; user : Sihl_user.t
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

let user_is_confirmed : type state. state t -> bool = function
  | Unverified { user; _ } | Verified { user; _ } -> user.Sihl_user.confirmed
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
    ; created_at = Ptime_clock.now ()
    ; updated_at = Ptime_clock.now ()
    }
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

type email = Sihl.Contract.Email.t

let email_of_yojson =
  CCFun.(
    Sihl.Contract.Email.of_yojson
    %> function
    | Some email -> email
    | None -> Yojson.json_error "Invalid serialized email string received")
;;

let yojson_of_email = Sihl.Contract.Email.to_yojson
let equal_email = Sihl.Contract.Email.equal
let pp_email = Sihl.Contract.Email.pp

type job =
  { email : email
  ; smtp_auth_id : SmtpAuth.Id.t option [@yojson.option]
  ; message_history : Queue.History.create option [@yojson.option]
  ; resent : Pool_common.Id.t option [@yojson.option]
  }
[@@deriving eq, show, yojson]

let parse_job_json str =
  try Ok (str |> Yojson.Safe.from_string |> job_of_yojson) with
  | _ -> Error Pool_message.(Error.Invalid Field.Input)
;;

let job_message_history { message_history; _ } = message_history

let create_job ?smtp_auth_id ?message_history email =
  { email; smtp_auth_id; message_history; resent = None }
;;
