open CCFun.Infix
open Sexplib.Conv
open Ppx_yojson_conv_lib.Yojson_conv

module Id = struct
  include Pool_model.Base.Id

  let of_common = Pool_common.Id.value %> of_string
  let to_common = value %> Pool_common.Id.of_string
end

module Status = struct
  module Core = struct
    let print = Utils.ppx_printer
    let field = Pool_message.Field.Status

    type t =
      | Active [@name "active"] [@printer print "active"]
      | Inactive [@name "inactive"] [@printer print "inactive"]
    [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
  end

  include Pool_model.Base.SelectorType (Core)
  include Core

  let to_human language =
    let go = Pool_common.Utils.field_to_string language in
    let open Pool_message in
    function
    | Active -> Field.Active |> go
    | Inactive -> Field.Inactive |> go
  ;;

  let read = Utils.Json.read_variant t_of_yojson

  let of_string str =
    try Ok (read str) with
    | _ -> Error Pool_message.(Error.Invalid Field.Status)
  ;;
end

module PasswordConfirmed = struct
  type t = string [@@deriving eq]

  let create m = m
  let show m = CCString.repeat "*" @@ CCString.length m

  let pp (formatter : Format.formatter) (m : t) : unit =
    m |> show |> Format.fprintf formatter "%s"
  ;;

  let schema ?(field = Pool_message.Field.PasswordConfirmation) () =
    Pool_conformist.schema_decoder (fun m -> Ok (create m)) show field
  ;;
end

module Password : sig
  include Pool_model.Base.StringSig

  module Policy : sig
    type rule =
      | MinLength of int
      | MustContainCapitalLetter
      | MustContainNumber
      | MustContainSpecialChar of char list

    type t = rule list

    val validate_min_length
      :  int
      -> string
      -> (string, Pool_conformist.error_msg) result

    val validate_characters
      :  (char -> bool)
      -> 'a
      -> string
      -> (string, 'a) result

    val validate_capital_letter
      :  string
      -> (string, Pool_conformist.error_msg) result

    val validate_number : string -> (string, Pool_conformist.error_msg) result

    val validate_special_char
      :  char list
      -> string
      -> (string, Pool_conformist.error_msg) result

    val default_special_char_set : char list
    val default : rule list

    val validate
      :  string
      -> rule list
      -> (string, Pool_conformist.error_msg) result
  end

  val schema
    :  ?field:Pool_message.Field.t
    -> (string -> (t, Pool_conformist.error_msg) result)
    -> unit
    -> ('a, t) Pool_conformist.Field.t

  val create_unvalidated : string -> (t, Pool_message.Error.t) result
  val to_confirmed : t -> PasswordConfirmed.t

  val validate_password_confirmation
    :  t
    -> PasswordConfirmed.t
    -> (unit, Pool_conformist.error_msg) result
end = struct
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

    let default =
      [ MinLength 8
      ; MustContainCapitalLetter
      ; MustContainNumber
      ; MustContainSpecialChar default_special_char_set
      ]
    ;;

    let validate password =
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

  include Pool_model.Base.String

  let show m = CCString.repeat "*" @@ CCString.length m

  let pp (formatter : Format.formatter) (m : t) : unit =
    Format.fprintf formatter "%s" m
  ;;

  let create password =
    (* TODO: Consider checking against old password *)
    Policy.validate password Policy.default
  ;;

  let create_unvalidated = CCResult.return
  let to_confirmed m = m

  let schema ?(field = Pool_message.Field.Password) create () =
    Pool_conformist.schema_decoder create show field
  ;;

  let validate_password_confirmation
    new_password
    (password_confirmation : PasswordConfirmed.t)
    =
    if equal new_password password_confirmation
    then Ok ()
    else Error Pool_message.Error.PasswordConfirmationDoesNotMatch
  ;;
end

module HashedPassword : sig
  include Pool_model.Base.StringSig

  val create : Password.t -> (t, Pool_message.Error.t) result
  val matches : t -> Password.t -> bool
end = struct
  module Hashing = struct
    let hash ?count plain =
      match count, not (Sihl.Configuration.is_production ()) with
      | _, true -> Ok (Bcrypt.hash ~count:4 plain |> Bcrypt.string_of_hash)
      | Some count, false ->
        if count < 4 || count > 31
        then Error Pool_message.Error.InvalidPasswordHashingCount
        else Ok (Bcrypt.hash ~count plain |> Bcrypt.string_of_hash)
      | None, false -> Ok (Bcrypt.hash ~count:10 plain |> Bcrypt.string_of_hash)
    ;;

    let matches ~hash ~plain = Bcrypt.verify plain (Bcrypt.hash_of_string hash)
  end

  include Pool_model.Base.String

  let create = Password.value %> Hashing.hash
  let schema = schema Pool_message.Field.Password ~validation:Hashing.hash

  let matches user_password plain_password =
    Hashing.matches ~hash:user_password ~plain:(Password.value plain_password)
  ;;
end

let remove_whitespaces =
  let open Re in
  replace_string (space |> compile) ~by:""
;;

module EmailAddress : sig
  include Pool_model.Base.StringSig

  val validate_characters : t -> (t, Pool_message.Error.t) result
  val strip_email_suffix : t -> t option

  val validate_suffix
    :  Settings.EmailSuffix.t list option
    -> t
    -> (unit, Pool_message.Error.t) result

  val validate
    :  Settings.EmailSuffix.t list option
    -> t
    -> (unit, Pool_message.Error.t) result
end = struct
  include Pool_model.Base.String

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
  let create = CCFun.(remove_whitespaces %> validate_characters)
  let schema = schema Pool_message.Field.Email ~validation:create
end

module CellPhone = struct
  include Pool_model.Base.String

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

  let schema_test_cell_phone =
    schema Pool_message.Field.TestPhoneNumber ~validation:create
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

module Firstname : sig
  include Pool_model.Base.StringSig
end = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.Firstname
  let schema () = schema field ()
  let of_string m = m
end

module Lastname : sig
  include Pool_model.Base.StringSig
end = struct
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

type t =
  { id : Id.t
  ; email : EmailAddress.t
  ; name : Lastname.t
  ; given_name : Firstname.t
  ; password : HashedPassword.t [@opaque] [@sexp.opaque]
  ; status : Status.t
  ; admin : bool
  ; confirmed : bool
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving yojson, show, sexp_of]

let equal a b =
  Id.equal a.id b.id
  && EmailAddress.equal a.email b.email
  && Status.equal a.status b.status
  && CCBool.equal a.admin b.admin
  && CCBool.equal a.confirmed b.confirmed
;;

let compare a b = EmailAddress.compare a.email b.email
let id { id; _ } = id
let user_email_address user = user.email
let user_firstname { given_name; _ } = given_name
let user_lastname { name; _ } = name

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

let confirm user = { user with confirmed = true }

let set_user_password user new_password =
  let open CCResult in
  HashedPassword.create new_password >|= fun password -> { user with password }
;;

let is_admin user = user.admin
let is_confirmed user = user.confirmed

let create
  ?(id = Id.create ())
  ?(admin = false)
  ?(confirmed = false)
  email
  name
  given_name
  password
  : (t, Pool_message.Error.t) result
  =
  let open CCResult.Infix in
  let* hash = HashedPassword.create password in
  Ok
    { id
    ; email
    ; password = hash
    ; name
    ; given_name
    ; admin
    ; confirmed
    ; status = Status.Active
    ; created_at = Pool_common.CreatedAt.create_now ()
    ; updated_at = Pool_common.UpdatedAt.create_now ()
    }
;;

let validate_current_password
  ?(field = Pool_message.Field.CurrentPassword)
  user
  password
  =
  if HashedPassword.matches user.password password
  then Ok ()
  else Error Pool_message.Error.(Invalid field)
;;

let validate_change_password
  (user : t)
  ~(old_password : Password.t)
  ~(new_password : Password.t)
  ~(new_password_confirmation : PasswordConfirmed.t)
  =
  let open CCResult.Infix in
  let* () = validate_current_password user old_password in
  Password.validate_password_confirmation new_password new_password_confirmation
;;

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
