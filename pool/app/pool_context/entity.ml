module PoolError = Pool_common.Message
open Sexplib.Conv

(* TODO: Service.User.t for Admin and Root are placeholders and should be
   replaced, when guadrian is implemented *)
type user =
  | Admin of Service.User.t
  | Contact of Contact.t
  | Root of Service.User.t
[@@deriving eq, sexp_of, variants]

type t =
  { query_language : Pool_common.Language.t option
  ; language : Pool_common.Language.t
  ; tenant_db : Pool_database.Label.t
  ; message : PoolError.Collection.t option
  ; csrf : string
  ; user : user option
  }
[@@deriving sexp_of]

let create (query_language, language, tenant_db, message, csrf, user) =
  { query_language; language; tenant_db; message; csrf; user }
;;

let find_context key req =
  Opium.Context.find key req.Opium.Request.env
  |> CCOption.to_result Pool_common.Message.PoolContextNotFound
  |> CCResult.map_err (fun err -> Pool_common.Utils.with_log_error err)
;;

let set_context key req context =
  let env = req.Opium.Request.env in
  let env = Opium.Context.add key context env in
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
  | Some (Contact c) -> Ok c
  | None | Some (Admin _) | Some (Root _) ->
    Error PoolError.(NotFound Field.User)
;;

module Tenant = struct
  type t =
    { tenant : Pool_tenant.t
    ; tenant_languages : Pool_common.Language.t list
    }
  [@@deriving sexp_of]

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
  | Admin user | Root user -> user.Sihl_user.email
  | Contact contact -> contact.Contact.user.Sihl_user.email
;;

let show_log (t : t) =
  Format.sprintf
    "%s %s"
    (CCOption.value ~default:"anonymous" @@ CCOption.map show_log_user t.user)
    (Pool_database.Label.show t.tenant_db)
;;
