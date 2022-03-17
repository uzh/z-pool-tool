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

let key : t Opium.Context.key =
  Opium.Context.Key.create ("tenant context", sexp_of_t)
;;

let find req =
  Opium.Context.find key req.Opium.Request.env
  |> CCOption.to_result Pool_common.Message.TenantContextNotFound
  |> CCResult.map_err (fun err -> Pool_common.Utils.with_log_error err)
;;

let find_exn req =
  match Opium.Context.find key req.Opium.Request.env with
  | Some context -> context
  | None -> failwith "Cannot find tenant context."
;;

let set req context =
  let env = req.Opium.Request.env in
  let env = Opium.Context.add key context env in
  Opium.Request.{ req with env }
;;
