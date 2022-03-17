module PoolError = Pool_common.Message
open Sexplib.Conv

type t =
  { query_language : Pool_common.Language.t option
  ; language : Pool_common.Language.t
  ; tenant_db : Pool_database.Label.t
  }
[@@deriving sexp_of]

let create (query_language, language, tenant_db) =
  { query_language; language; tenant_db }
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
end
