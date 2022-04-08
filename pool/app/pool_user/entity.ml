module PoolError = Pool_common.Message

module Password = struct
  type t = string [@@deriving eq]

  let default_password_policy p =
    if CCString.length p < 8 then Error "password_policy_text" else Ok ()
  ;;

  let validate ?(password_policy = default_password_policy) password =
    let open CCResult in
    let* () =
      password_policy password |> map_err Pool_common.Message.passwordpolicy
    in
    Ok ()
  ;;

  let create password = Ok password
  let to_sihl m = m
  let show m = CCString.repeat "*" @@ CCString.length m

  let pp (formatter : Format.formatter) (m : t) : unit =
    Format.fprintf formatter "%s" m
  ;;

  let schema ?(field = PoolError.Password) () =
    Pool_common.Utils.schema_decoder create show field
  ;;
end

module PasswordConfirmed = struct
  type t = string [@@deriving eq]

  let create m = m
  let to_sihl m = m
  let show m = CCString.repeat "*" @@ CCString.length m

  let pp (formatter : Format.formatter) (m : t) : unit =
    m |> show |> Format.fprintf formatter "%s"
  ;;

  let schema ?(field = PoolError.PasswordConfirmation) () =
    Pool_common.Utils.schema_decoder (fun m -> Ok (create m)) show field
  ;;
end

module EmailAddress = struct
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
    if Re.execp regex email
    then Ok email
    else Error PoolError.(Invalid EmailAddress)
  ;;

  let strip_email_suffix email =
    (* TODO check whether this is stable *)
    let tail = CCString.split_on_char '@' email |> CCList.tail_opt in
    CCOption.bind tail CCList.head_opt
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
      | None -> Error PoolError.EmailMalformed
      | Some suffix ->
        let open CCResult in
        let* suffix = suffix |> Settings.EmailSuffix.create in
        if CCList.mem
             ~eq:Settings.EmailSuffix.equal
             suffix
             allowed_email_suffixes
        then Ok ()
        else Error PoolError.(Invalid EmailSuffix))
  ;;

  let validate = validate_suffix
  let value m = m
  let create email = email |> remove_whitespaces |> validate_characters
  let of_string m = m
  let schema () = Pool_common.Utils.schema_decoder create show PoolError.Email
end

module Firstname = struct
  type t = string [@@deriving eq, show]

  let create m =
    if CCString.is_empty m
    then Error Pool_common.Message.(Invalid Firstname)
    else Ok m
  ;;

  let of_string m = m
  let value m = m

  let schema () =
    Pool_common.Utils.schema_decoder create value PoolError.Firstname
  ;;
end

module Lastname = struct
  type t = string [@@deriving eq, show]

  let create m =
    if CCString.is_empty m
    then Error Pool_common.Message.(Invalid Lastname)
    else Ok m
  ;;

  let of_string m = m
  let value m = m

  let schema () =
    Pool_common.Utils.schema_decoder create value PoolError.Lastname
  ;;
end

module Paused = struct
  type t = bool [@@deriving eq, show]

  let create m = m
  let value m = m

  let schema () =
    Pool_common.Utils.schema_decoder
      (fun m ->
        m
        |> bool_of_string_opt
        |> CCOption.get_or ~default:false
        |> CCResult.pure)
      string_of_bool
      PoolError.Paused
  ;;
end

module Disabled = struct
  type t = bool [@@deriving eq, show]

  let create m = m
  let value m = m
end

module TermsAccepted = struct
  type t = Ptime.t option [@@deriving eq, show]

  let create m = m
  let create_now () = Some (Ptime_clock.now ())
  let value m = m
end

module Verified = struct
  type t = Ptime.t option [@@deriving eq, show]

  let create m = m
  let create_now () = Some (Ptime_clock.now ())
  let value m = m
end
