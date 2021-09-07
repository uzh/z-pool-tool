module Token = struct
  type t = string [@@deriving eq, show]

  let t = Caqti_type.string
end

module Address = struct
  type t = string [@@deriving eq, show]

  let create email =
    let open Re in
    (* Removes all whitespaces from the email address and checks for more than 1
       character before and more than 2 characters after the @ sign *)
    let email = replace_string (space |> compile) ~by:"" email in
    let regex =
      seq [ repn any 1 None; char '@'; repn any 2 None ]
      |> whole_string
      |> compile
    in
    if Re.execp regex email then Ok email else Error "Invalid email address!"
  ;;
end

module VerifiedAt = struct
  type t = Ptime.t [@@deriving eq, show]
end

type email_unverified =
  { address : Address.t
  ; token : Token.t
  }
[@@deriving eq, show]

type email_verified =
  { address : Address.t
  ; verified_at : VerifiedAt.t
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

let equal : type email. email t -> email t -> bool =
 fun m k ->
  match m, k with
  | Unverified one, Unverified two -> equal_email_unverified one two
  | Verified one, Verified two -> equal_email_verified one two
;;

let pp : type email. Format.formatter -> email t -> unit =
 fun formatter email ->
  match email with
  | Unverified m -> pp_email_unverified formatter m
  | Verified m -> pp_email_verified formatter m
;;

let show : type state. state t -> 'a = function
  | Unverified m -> Address.show m.address
  | Verified m -> Address.show m.address
;;

let token (Unverified email) = Token.show email.token

let create email token =
  let open CCResult.Infix in
  email |> Address.create >|= fun address -> Unverified { address; token }
;;

let verify (Unverified email) =
  Verified { address = email.address; verified_at = Ptime_clock.now () }
;;
