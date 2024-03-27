open Ppx_yojson_conv_lib.Yojson_conv

module PasswordConfirmed = struct
  type t = string [@@deriving eq]

  let create m = m
  let to_sihl m = m
  let show m = CCString.repeat "*" @@ CCString.length m

  let pp (formatter : Format.formatter) (m : t) : unit =
    m |> show |> Format.fprintf formatter "%s"
  ;;

  let schema ?(field = Pool_message.Field.PasswordConfirmation) () =
    Pool_conformist.schema_decoder (fun m -> Ok (create m)) show field
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
      then Error (Pool_message.Error.PasswordPolicyMinLength num)
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
      validate_characters
        validate
        Pool_message.Error.PasswordPolicyCapitalLetter
    ;;

    let validate_number =
      let validate c = CCChar.to_int c >= 48 && CCChar.to_int c <= 57 in
      validate_characters validate Pool_message.Error.PasswordPolicyNumber
    ;;

    let validate_special_char chars p =
      chars
      |> CCList.fold_left (fun is_ok c -> is_ok || CCString.contains p c) false
      |> function
      | true -> Ok p
      | false -> Error (Pool_message.Error.PasswordPolicySpecialChar chars)
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

  let schema ?(field = Pool_message.Field.Password) create () =
    Pool_conformist.schema_decoder create show field
  ;;

  let validate_current_password
    ?(field = Pool_message.Field.CurrentPassword)
    user
    password
    =
    if Sihl_user.matches_password (password |> to_sihl) user
    then Ok ()
    else Error Pool_message.Error.(Invalid field)
  ;;

  let validate_password_confirmation new_password password_confirmation =
    if equal new_password (PasswordConfirmed.to_sihl password_confirmation)
    then Ok ()
    else Error Pool_message.Error.PasswordConfirmationDoesNotMatch
  ;;
end

let remove_whitespaces =
  let open Re in
  replace_string (space |> compile) ~by:""
;;

module EmailAddress = struct
  type t = string [@@deriving eq, show, yojson]

  let validate_characters email =
    let open Mrmime in
    match Mailbox.of_string email with
    | Ok _ -> Ok email
    | Error _ -> Error Pool_message.(Error.Invalid Field.EmailAddress)
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
       | None -> Error Pool_message.Error.EmailMalformed
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
             Pool_message.Error.(
               InvalidEmailSuffix
                 (allowed_email_suffixes
                  |> CCList.map Settings.EmailSuffix.value)))
  ;;

  let validate = validate_suffix
  let value m = m
  let create = CCFun.(remove_whitespaces %> validate_characters)
  let of_string m = m

  let schema () =
    Pool_conformist.schema_decoder create show Pool_message.Field.Email
  ;;
end

module CellPhone = struct
  type t = string [@@deriving eq, ord, show, yojson]

  let validate str =
    let regex =
      Re.(
        seq [ alt [ char '+'; char '0' ]; repn digit 7 (Some 16) ]
        |> whole_string
        |> compile)
    in
    if Re.execp regex str
    then Ok str
    else Error Pool_message.(Error.Invalid Field.CellPhone)
  ;;

  let create = CCFun.(remove_whitespaces %> validate)
  let of_string m = m
  let value m = m

  let schema_test_cell_phone () =
    Pool_conformist.schema_decoder
      create
      show
      Pool_message.Field.TestPhoneNumber
  ;;
end

module UnverifiedCellPhone = struct
  type t =
    { cell_phone : CellPhone.t
    ; created_at : Pool_common.CreatedAt.t
    }

  type full =
    { cell_phone : CellPhone.t
    ; verification_code : Pool_common.VerificationCode.t
    ; created_at : Pool_common.CreatedAt.t
    }
end

module Firstname = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.Firstname
  let schema () = schema field ()
  let of_string m = m
end

module Lastname = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.Lastname
  let schema () = schema field ()
  let of_string m = m
end

module Paused = struct
  include Pool_model.Base.Boolean

  let schema = schema Pool_message.Field.Paused
end

module Disabled = struct
  type t = bool [@@deriving eq, ord, show]

  let create m = m
  let value m = m
end

module TermsAccepted = struct
  include Pool_model.Base.Ptime

  let create m = m
end

module Verified = struct
  include Pool_model.Base.Ptime

  let create m = m
end

module EmailVerified = struct
  include Pool_model.Base.Ptime

  let create m = m
end

module ImportPending = struct
  include Pool_model.Base.Boolean

  let schema = schema Pool_message.Field.ImportPending
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

let user_lastname_firstname user =
  Format.asprintf
    "%s %s"
    (user |> user_lastname |> Lastname.value)
    (user |> user_firstname |> Firstname.value)
;;

let user_email_address user = user.Sihl_user.email |> EmailAddress.of_string

open Pool_message

let column_email = (Field.Email, "user_users.email") |> Query.Column.create

let column_first_name =
  (Field.Firstname, "user_users.given_name") |> Query.Column.create
;;

let column_last_name =
  (Field.Lastname, "user_users.name") |> Query.Column.create
;;

let column_name =
  (Field.Name, "CONCAT_WS(' ', user_users.name, user_users.given_name)")
  |> Query.Column.create
;;

let column_inactive =
  Query.Column.create
    ( Field.HideInactive
    , Format.asprintf "user_users.status != '%s'" Field.(show Inactive) )
;;

let searchable_and_sortable_by =
  [ Field.Email, "user_users.email"
  ; Field.Firstname, "user_users.given_name"
  ; Field.Lastname, "user_users.name"
  ]
;;

let searchable_by =
  ( Field.Name
  , "CONCAT_WS(' ', user_users.name, user_users.given_name, user_users.name)" )
  :: searchable_and_sortable_by
  |> Query.Column.create_list
;;

let sortable_by =
  column_name
  :: (searchable_and_sortable_by
      @ [ Field.CreatedAt, "pool_contacts.created_at" ]
      |> Query.Column.create_list)
;;

let default_sort =
  let open Query in
  Sort.{ column = column_name; order = SortOrder.Ascending }
;;

let default_query = Query.create ~sort:default_sort ()
