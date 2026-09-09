open CCFun.Infix
open Entity

module Tags = struct
  let add_label : string Logs.Tag.def =
    Logs.Tag.def "database_label" ~doc:"Database Label" CCString.pp
  ;;

  let add = Label.value %> Logs.Tag.add add_label
  let create database = Logs.Tag.empty |> add database
  let extend label = CCOption.map_or ~default:(create label) (add label)

  let of_db_ctx (type maybe_txn) : maybe_txn ctx -> Logs.Tag.set = function
    | Label { tags; _ } | Connection { tags; _ } | TransactionalConnection { tags; _ } ->
      tags
  let merge ctx tags =
    Logs.Tag.fold (fun (Logs.Tag.V (k, v)) acc -> Logs.Tag.add k v acc)
      (of_db_ctx ctx) tags
end
