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
  module Policy = struct
    type rule =
      | MinLength of int
      | MustContainCapitalLetter
      | MustContainNumber
      | MustContainSpecialChar of char list

    type t = rule list

    let validate_min_length num p =
      if CCString.length p < num
      then Error (PoolError.PasswordPolicyMinLength num)
      else Ok p
    ;;

    let validate_characters validator error p =
      p
      |> CCString.to_list
      |> CCList.fold_left (fun is_ok c -> is_ok || validator c) false
      |> function
      | true -> Ok p
      | false -> Error error
    ;;

    let validate_capital_letter =
      let validate c = CCChar.to_int c >= 65 && CCChar.to_int c <= 90 in
      validate_characters validate PoolError.PasswordPolicyCapitalLetter
    ;;

    let validate_number =
      let validate c = CCChar.to_int c >= 48 && CCChar.to_int c <= 57 in
      validate_characters validate PoolError.PasswordPolicyNumber
    ;;

    let validate_special_char chars p =
      chars
      |> CCList.fold_left (fun is_ok c -> is_ok || CCString.contains p c) false
      |> function
      | true -> Ok p
      | false -> Error (PoolError.PasswordPolicySpecialChar chars)
    ;;

    let default_special_char_set =
      [ '!'
      ; '?'
      ; '*'
      ; '+'
      ; '-'
      ; '_'
      ; '&'
      ; '%'
      ; '('
      ; ')'
      ; '}'
      ; '{'
      ; '$'
      ; ','
      ; '.'
      ]
    ;;

    let default_policy =
      [ MinLength 8
      ; MustContainCapitalLetter
      ; MustContainNumber
      ; MustContainSpecialChar default_special_char_set
      ]
    ;;

    let valdate password =
      let open CCResult in
      CCList.fold_left
        (fun password rule ->
          password
          >>=
          match rule with
          | MinLength n -> validate_min_length n
          | MustContainCapitalLetter -> validate_capital_letter
          | MustContainNumber -> validate_number
          | MustContainSpecialChar chars -> validate_special_char chars)
        (Ok password)
    ;;
  end

  type t = string [@@deriving eq]

  let to_sihl m = m
  let show m = CCString.repeat "*" @@ CCString.length m

  let pp (formatter : Format.formatter) (m : t) : unit =
    Format.fprintf formatter "%s" m
  ;;

  let create password =
    (* TODO: Consider checking against old password *)
    Policy.valdate password Policy.default_policy
  ;;

  let create_unvalidated p = Ok p

  let schema ?(field = PoolError.Field.Password) create () =
    Pool_common.Utils.schema_decoder create show field
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
         else
           Error
             PoolError.(
               InvalidEmailSuffix
                 (allowed_email_suffixes
                  |> CCList.map Settings.EmailSuffix.value)))
  ;;

  let validate = validate_suffix
  let value m = m
  let create = CCFun.(remove_whitespaces %> validate_characters)
  let of_string m = m

  let schema () =
    Pool_common.Utils.schema_decoder create show PoolError.Field.Email
  ;;
end

module Firstname = struct
  include Pool_common.Model.String

  let field = PoolError.Field.Firstname
  let schema () = schema field ()
  let of_string m = m
end

module Lastname = struct
  include Pool_common.Model.String

  let field = PoolError.Field.Lastname
  let schema () = schema field ()
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
  include Pool_common.Model.Ptime

  let create m = m
end

module Verified = struct
  include Pool_common.Model.Ptime

  let create m = m
end

module EmailVerified = struct
  include Pool_common.Model.Ptime

  let create m = m
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

let user_email_address user = user.Sihl_user.email |> EmailAddress.of_string
