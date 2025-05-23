open Sexplib.Conv

(* TODO: Service.User.t for Admin and Root are placeholders and should be replaced, when
   guadrian is implemented *)
type user =
  | Admin of Admin.t
  | Contact of Contact.t
  | Guest
[@@deriving eq, show, sexp_of, variants]

module Notitification = struct
  type tenant =
    { hint : Pool_common.I18n.hint
    ; style : [ `Error | `Success | `Warning ]
    ; link : (string * Pool_common.I18n.nav_link) option
    }
  [@@deriving eq, show, sexp_of]

  type t =
    | Root of Announcement.t
    | Tenant of tenant
  [@@deriving eq, show, sexp_of]
end

module UserType = struct
  type t =
    | Admin
    | Contact
    | Guest

  let user_in roles = function
    | (Guest : user) -> CCList.mem (Guest : t) roles
    | Contact _ -> CCList.mem (Contact : t) roles
    | Admin _ -> CCList.mem (Admin : t) roles
  ;;
end

type t =
  { query_parameters : (Pool_message.Field.t * string) list
  ; language : Pool_common.Language.t
  ; database_label : Database.Label.t
  ; message : Pool_message.Collection.t option
  ; csrf : string
  ; user : user
  ; guardian : Guard.PermissionOnTarget.t list [@sexp.list]
  ; notifications : Notitification.t list
  ; flash_fetcher : (string -> string option) option
  }
[@@deriving show, sexp_of]

let create
      ( query_parameters
      , language
      , database_label
      , message
      , csrf
      , user
      , guardian
      , notifications )
  =
  { query_parameters
  ; language
  ; database_label
  ; message
  ; csrf
  ; user
  ; guardian
  ; notifications
  ; flash_fetcher = None
  }
;;

let find_context key req =
  Opium.Context.find key req.Opium.Request.env
  |> CCOption.to_result Pool_message.Error.PoolContextNotFound
;;

let set_context key req context =
  let env = Opium.Context.add key context req.Opium.Request.env in
  Opium.Request.{ req with env }
;;

let key : t Opium.Context.key = Opium.Context.Key.create ("pool context", sexp_of_t)
let find = find_context key

let find_exn req =
  match Opium.Context.find key req.Opium.Request.env with
  | Some context -> context
  | None -> failwith "Cannot find tenant context."
;;

let set = set_context key

let find_contact { user; _ } =
  match user with
  | Contact c -> Ok c
  | Admin _ | Guest -> Error Pool_message.(Error.NotFound Field.Contact)
;;

let context_user_of_user database_label user =
  let open Utils.Lwt_result.Infix in
  if Pool_user.is_admin user
  then
    user.Pool_user.id
    |> Admin.Id.of_user
    |> Admin.find database_label
    ||> function
    | Ok user -> user |> admin
    | Error _ -> Guest
  else
    Contact.find_by_user database_label user
    ||> CCResult.to_opt
    ||> CCOption.map_or ~default:Guest contact
;;

let dashboard_path ?(guest = "/index") = function
  | Admin _ -> "/admin/dashboard"
  | Contact _ -> "/experiments"
  | Guest -> guest
;;

module Tenant = struct
  type t =
    { tenant : Pool_tenant.t
    ; tenant_languages : Pool_common.Language.t list
    }
  [@@deriving show, sexp_of]

  let create tenant tenant_languages = { tenant; tenant_languages }
  let key : t Opium.Context.key = Opium.Context.Key.create ("tenant context", sexp_of_t)
  let find = find_context key
  let set = set_context key

  let find_key_exn fnc req =
    let open CCResult in
    req |> find >|= fnc |> Pool_common.Utils.get_or_failwith
  ;;

  let get_tenant_languages_exn = find_key_exn (fun c -> c.tenant_languages)
  let get_tenant_exn = find_key_exn (fun c -> c.tenant)

  let text_messages_enabled =
    find_key_exn (fun c ->
      Gtx_config.text_messages_enabled c.tenant.Pool_tenant.database_label)
  ;;
end

module Api = struct
  type t =
    { api_key : Api_key.t
    ; database_label : Database.Label.t
    ; guardian : Guard.PermissionOnTarget.t list [@sexp.list]
    }
  [@@deriving show, sexp_of]

  let key : t Opium.Context.key = Opium.Context.Key.create ("api context", sexp_of_t)
  let create api_key database_label guardian = { api_key; database_label; guardian }
  let find = find_context key
  let set = set_context key
end

(* Logging *)
let show_log_user = function
  | Admin user -> user.Admin.user.Pool_user.email |> Pool_user.EmailAddress.value
  | Contact contact ->
    contact.Contact.user.Pool_user.email |> Pool_user.EmailAddress.value
  | Guest -> "anonymous"
;;
