module PoolError = Pool_common.Message
open Sexplib.Conv

(* TODO: Service.User.t for Admin and Root are placeholders and should be
   replaced, when guadrian is implemented *)
type user =
  | Admin of Admin.t
  | Contact of Contact.t
  | Guest
[@@deriving eq, show, sexp_of, variants]

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
  { query_language : Pool_common.Language.t option
  ; language : Pool_common.Language.t
  ; database_label : Pool_database.Label.t
  ; message : PoolError.Collection.t option
  ; csrf : string
  ; user : user
  }
[@@deriving show, sexp_of]

let create (query_language, language, database_label, message, csrf, user) =
  { query_language; language; database_label; message; csrf; user }
;;

let find_context key req =
  Opium.Context.find key req.Opium.Request.env
  |> CCOption.to_result Pool_common.Message.PoolContextNotFound
;;

let set_context key req context =
  let env = Opium.Context.add key context req.Opium.Request.env in
  Opium.Request.{ req with env }
;;

let key : t Opium.Context.key =
  Opium.Context.Key.create ("pool context", sexp_of_t)
;;

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
  | Admin _ | Guest -> Error PoolError.(NotFound Field.User)
;;

let user_of_sihl_user database_label user =
  let open Utils.Lwt_result.Infix in
  if Sihl_user.is_admin user
  then user |> Admin.create |> admin |> Lwt.return
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

let find_authenticatable { user; database_label; _ } =
  let ctx = Pool_tenant.to_ctx database_label in
  match user with
  | Admin admin ->
    admin
    |> Admin.user
    |> (fun { Sihl_user.id; _ } -> Guard.Uuid.Actor.of_string_exn id)
    |> Guard.Persistence.Actor.find_authorizable ~ctx `Admin
    |> Lwt_result.map_error PoolError.authorization
  | Contact c -> Contact.Guard.Actor.to_authorizable ~ctx c
  | Guest -> Lwt.return_error PoolError.(NotFound Field.User)
;;

module Tenant = struct
  type t =
    { tenant : Pool_tenant.t
    ; tenant_languages : Pool_common.Language.t list
    }
  [@@deriving show, sexp_of]

  let create tenant tenant_languages = { tenant; tenant_languages }

  let key : t Opium.Context.key =
    Opium.Context.Key.create ("tenant context", sexp_of_t)
  ;;

  let find = find_context key
  let set = set_context key

  let get_tenant_languages req =
    let open CCResult in
    req |> find >|= fun c -> c.tenant_languages
  ;;
end

(* Logging *)
let show_log_user = function
  | Admin user -> user |> Admin.user |> fun user -> user.Sihl_user.email
  | Contact contact -> contact.Contact.user.Sihl_user.email
  | Guest -> "anonymous"
;;

let show_log ({ database_label; user; _ } : t) =
  Format.sprintf
    "%s %s"
    (show_log_user user)
    (Pool_database.Label.value database_label)
;;
