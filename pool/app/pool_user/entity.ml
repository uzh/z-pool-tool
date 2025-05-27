open CCFun.Infix

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

module EmailAddress = struct
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

  let validate_suffix (allowed_email_suffixes : Settings.EmailSuffix.t list option) email =
    match allowed_email_suffixes with
    | None -> Ok ()
    | Some allowed_email_suffixes ->
      (match strip_email_suffix email with
       (* TODO check whether this is really the case *)
       | None -> Error Pool_message.Error.EmailMalformed
       | Some suffix ->
         let open CCResult in
         let* suffix = suffix |> Settings.EmailSuffix.create in
         if CCList.mem ~eq:Settings.EmailSuffix.equal suffix allowed_email_suffixes
         then Ok ()
         else
           Error
             Pool_message.Error.(
               InvalidEmailSuffix
                 (allowed_email_suffixes |> CCList.map Settings.EmailSuffix.value)))
  ;;

  let validate = validate_suffix
  let create = CCFun.(Utils.remove_whitespaces %> validate_characters)
  let schema ?(field = Pool_message.Field.Email) = schema field ~validation:create
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

  let create = CCFun.(Utils.remove_whitespaces %> validate)
  let schema = schema Pool_message.Field.TestPhoneNumber ~validation:create
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
  include Pool_model.Base.Boolean

  let schema = schema Pool_message.Field.Disabled
end

module TermsAccepted = struct
  include Pool_model.Base.Ptime

  let schema = schema Pool_message.Field.TermsAccepted CCResult.return
end

module Verified = struct
  include Pool_model.Base.Ptime

  let schema = schema Pool_message.Field.Verified CCResult.return
  let equal a b = Sihl.Configuration.is_test () || equal a b
end

module EmailVerified = struct
  include Pool_model.Base.Ptime

  let schema = schema Pool_message.Field.EmailAddressVerified CCResult.return
end

module ImportPending = struct
  include Pool_model.Base.Boolean

  let schema = schema Pool_message.Field.ImportPending
end

module IsAdmin = struct
  include Pool_model.Base.Boolean

  let schema = schema Pool_message.Field.IsAdmin
end

module Confirmed = struct
  include Pool_model.Base.Boolean

  let schema = schema Pool_message.Field.Confirmed
end

type t =
  { id : Id.t
  ; email : EmailAddress.t
  ; lastname : Lastname.t
  ; firstname : Firstname.t
  ; status : Status.t
  ; admin : IsAdmin.t
  ; confirmed : Confirmed.t
  }
[@@deriving eq, fields, ord, show, sexp_of, yojson]

let fullname ?(reversed = false) user =
  [ user |> firstname |> Firstname.value; user |> lastname |> Lastname.value ]
  |> (if reversed then CCList.rev else CCFun.id)
  |> CCString.concat " "
;;

let is_admin user = user.admin |> IsAdmin.value
let is_confirmed user = user.confirmed |> Confirmed.value

let create
      ?(id = Id.create ())
      ?(admin = IsAdmin.create false)
      ?(confirmed = Confirmed.create false)
      email
      lastname
      firstname
  : (t, Pool_message.Error.t) result
  =
  Ok { id; email; lastname; firstname; admin; confirmed; status = Status.Active }
;;

let confirm user = { user with confirmed = Confirmed.create true }
let promote user = { user with admin = IsAdmin.create true }

let update ?email ?lastname ?firstname ?status ?confirmed user =
  { user with
    email = CCOption.value ~default:user.email email
  ; lastname = CCOption.value ~default:user.lastname lastname
  ; firstname = CCOption.value ~default:user.firstname firstname
  ; status = CCOption.value ~default:user.status status
  ; confirmed = CCOption.value ~default:user.confirmed confirmed
  }
;;

open Pool_message

let column_email = (Field.Email, "user_users.email") |> Query.Column.create
let column_first_name = (Field.Firstname, "user_users.given_name") |> Query.Column.create
let column_last_name = (Field.Lastname, "user_users.name") |> Query.Column.create

let column_name =
  (Field.Name, "CONCAT_WS(' ', user_users.name, user_users.given_name)")
  |> Query.Column.create
;;

let column_inactive =
  Query.Column.create
    (Field.HideInactive, Format.asprintf "user_users.status != '%s'" Field.(show Inactive))
;;

let searchable_and_sortable_by =
  [ Field.Email, "user_users.email"
  ; Field.Firstname, "user_users.given_name"
  ; Field.Lastname, "user_users.name"
  ]
;;

let searchable_by =
  (Field.Name, "CONCAT_WS(' ', user_users.name, user_users.given_name, user_users.name)")
  :: searchable_and_sortable_by
  |> Query.Column.create_list
;;

let sortable_by =
  column_name
  :: (searchable_and_sortable_by @ [ Field.CreatedAt, "pool_contacts.created_at" ]
      |> Query.Column.create_list)
;;

let default_sort =
  let open Query in
  Sort.{ column = column_name; order = SortOrder.Ascending }
;;

let default_query = Query.create ~sort:default_sort ()
