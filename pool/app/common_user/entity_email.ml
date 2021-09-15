module Token = struct
  type t = string [@@deriving eq, show]

  let t = Caqti_type.string
end

module Address = struct
  type t = string [@@deriving eq, show]

  let remove_whitespaces =
    let open Re in
    replace_string (space |> compile) ~by:""
  ;;

  let validate_characters email =
    let open Re in
    (* Checks for more than 1 character before and more than 2 characters after
       the @ sign *)
    let regex =
      seq [ repn any 1 None; char '@'; repn any 2 None ]
      |> whole_string
      |> compile
    in
    if Re.execp regex email then Ok email else Error "Invalid email provided"
  ;;

  let strip_email_suffix email =
    (* TODO check whether this is stable *)
    let tail = CCString.split_on_char '@' email |> CCList.tail_opt in
    CCOpt.bind tail CCList.head_opt
  ;;

  let validate_suffix
      (allowed_email_suffixes : Settings.EmailSuffix.t list option)
      email
    =
    match allowed_email_suffixes with
    | None -> Ok ()
    | Some allowed_email_suffixes ->
      (match strip_email_suffix email with
      (* TODO check whether this is really the case *)
      | None -> Error "Email malformed"
      | Some suffix ->
        if CCList.mem ~eq:String.equal suffix allowed_email_suffixes
        then Ok ()
        else Error "Invalid email suffix provided")
  ;;

  let validate = validate_suffix
  let create email = email |> remove_whitespaces |> validate_characters

  let schema () =
    Conformist.custom
      (fun l -> l |> List.hd |> create)
      (fun l -> [ show l ])
      "email"
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
let create address token = Ok (Unverified { address; token })

let verify (Unverified email) =
  Verified { address = email.address; verified_at = Ptime_clock.now () }
;;
