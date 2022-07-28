module PoolError = Pool_common.Message
module Database = Pool_database
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

(* TODO: Add name to use instead of Pool Tool *)
type email_layout =
  { link : string
  ; logo_src : string
  ; logo_alt : string
  }
[@@deriving eq, show { with_path = false }]

module TemplateLabel = struct
  let go m fmt _ = Format.pp_print_string fmt m

  type t =
    | Boilerplate [@name "boilerplate"] [@printer go "boilerplate"]
    | EmailVerification [@name "email_verification"]
        [@printer go "email_verification"]
    | Invitation [@name "invitation"] [@printer go "invitation"]
    | PasswordChange [@name "password_change"] [@printer go "password_change"]
    | PasswordReset [@name "password_reset"] [@printer go "password_reset"]
    | SignUpVerification [@name "signup_verification"]
    | SessionCancellation [@name "session_cancellation"]
        [@printer go "session_cancellation"]
  [@@deriving eq, show { with_path = false }, yojson, variants]

  let read m =
    m |> Format.asprintf "[\"%s\"]" |> Yojson.Safe.from_string |> t_of_yojson
  ;;
end

module CustomTemplate = struct
  module Subject = struct
    type t =
      | I18n of I18n.t
      | String of string
    [@@deriving eq, show, variants]

    let value = function
      | I18n s -> I18n.content_to_string s
      | String s -> s
    ;;
  end

  module Content = struct
    type t =
      | I18n of I18n.t
      | String of string
    [@@deriving eq, show, variants]

    let value = function
      | I18n c -> I18n.content_to_string c
      | String c -> c
    ;;
  end

  type t =
    { subject : Subject.t
    ; content : Content.t
    ; layout : email_layout
    }
  [@@deriving eq, show]
end

type text_component = (string, string) CCPair.t [@@deriving eq, show]

module Token = struct
  type t = string [@@deriving eq, show]

  let create m = m
  let value m = m
end

module VerifiedAt = struct
  include Pool_common.Model.Ptime

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
    ; token
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
