module PoolError = Pool_common.Message

module PasswordConfirmed = struct
  type t = string [@@deriving eq]

  let create m = m
  let to_sihl m = m
  let show m = CCString.repeat "*" @@ CCString.length m

  let pp (formatter : Format.formatter) (m : t) : unit =
    m |> show |> Format.fprintf formatter "%s"
  ;;

  let schema ?(field = PoolError.Field.PasswordConfirmation) () =
    Pool_common.Utils.schema_decoder (fun m -> Ok (create m)) show field
  ;;
end

module Password = struct
  type t = string [@@deriving eq]

  let create password = Ok password
  let to_sihl m = m
  let show m = CCString.repeat "*" @@ CCString.length m

  let pp (formatter : Format.formatter) (m : t) : unit =
    Format.fprintf formatter "%s" m
  ;;

  let schema ?(field = PoolError.Field.Password) () =
    Pool_common.Utils.schema_decoder create show field
  ;;

  let default_password_policy p =
    if CCString.length p < 8 then Error PoolError.PasswordPolicy else Ok ()
  ;;

  let validate ?(password_policy = default_password_policy) password =
    (* TODO: Consider checking against old password *)
    password |> password_policy
  ;;

  let validate_current_password
    ?(field = PoolError.Field.CurrentPassword)
    user
    password
    =
    if Sihl_user.matches_password (password |> to_sihl) user
    then Ok ()
    else Error PoolError.(Invalid field)
  ;;

  let validate_password_confirmation new_password password_confirmation =
    if equal new_password (PasswordConfirmed.to_sihl password_confirmation)
    then Ok ()
    else Error PoolError.PasswordConfirmationDoesNotMatch
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
    else Error PoolError.(Invalid Field.EmailAddress)
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
         else Error PoolError.(Invalid Field.EmailSuffix))
  ;;

  let validate = validate_suffix
  let value m = m
  let create email = email |> remove_whitespaces |> validate_characters
  let of_string m = m

  let schema () =
    Pool_common.Utils.schema_decoder create show PoolError.Field.Email
  ;;
end

module Firstname = struct
  include Pool_common.Model.String

  let field = PoolError.Field.Firstname
  let create = create field
  let schema = schema ?validation:None field
  let of_string m = m
end

module Lastname = struct
  include Pool_common.Model.String

  let field = PoolError.Field.Lastname
  let create = create field
  let schema = schema ?validation:None field
  let of_string m = m
end

module Paused = struct
  include Pool_common.Model.Boolean

  let schema = schema PoolError.Field.Paused
end

module Disabled = struct
  type t = bool [@@deriving eq, show]

  let create m = m
  let value m = m
end

module TermsAccepted = struct
  type t = Ptime.t [@@deriving eq, show]

  let create m = m
  let create_now () = Ptime_clock.now ()
  let value m = m
end

module Verified = struct
  type t = Ptime.t [@@deriving eq, show]

  let create m = m
  let create_now () = Ptime_clock.now ()
  let value m = m
end

module EmailVerified = struct
  type t = Ptime.t [@@deriving eq, show]

  let create m = m
  let create_now () = Ptime_clock.now ()
  let value m = m
end

let user_firstname { Sihl_user.id; given_name; _ } =
  given_name
  |> CCOption.get_exn_or (Format.asprintf "User '%s' has no firstname" id)
  |> Firstname.of_string
;;

let user_lastname { Sihl_user.id; name; _ } =
  name
  |> CCOption.get_exn_or (Format.asprintf "User '%s' has no firstname" id)
  |> Lastname.of_string
;;

let user_fullname user =
  Format.asprintf
    "%s %s"
    (user |> user_firstname |> Firstname.value)
    (user |> user_lastname |> Lastname.value)
;;
