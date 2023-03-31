module Conformist = Pool_common.Utils.PoolConformist
module Id = Pool_common.Id

let src = Logs.Src.create "filter.cqrs"

let default_schema command =
  Pool_common.Utils.PoolConformist.(
    make Field.[ Filter.Title.schema () ] command)
;;

let default_command = CCFun.id

let default_decode data =
  Conformist.decode_and_validate (default_schema default_command) data
  |> CCResult.map_err Pool_common.Message.to_conformist_error
;;

let validate_query key_list template_list query =
  let open CCResult in
  let* query = Filter.validate_query key_list template_list query in
  let* query =
    if Filter.contains_template query
    then Error Pool_common.Message.FilterMustNotContainTemplate
    else Ok query
  in
  Ok query
;;

module Create : sig
  include Common.CommandSig with type t = Filter.Title.t

  val handle
    :  ?tags:Logs.Tag.set
    -> Filter.Key.human list
    -> Filter.t list
    -> Filter.query
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result
end = struct
  type t = Filter.Title.t

  let handle ?(tags = Logs.Tag.empty) key_list template_list query title =
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
    let open CCResult in
    let* query = validate_query key_list template_list query in
    Ok
      [ Filter.Created (Filter.create (Some title) query) |> Pool_event.filter ]
  ;;

  let effects = Filter.Guard.Access.create
end

module Update : sig
  include Common.CommandSig with type t = Filter.Title.t

  val handle
    :  ?tags:Logs.Tag.set
    -> Filter.Key.human list
    -> Filter.t list
    -> Filter.t
    -> Filter.query
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Filter.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Filter.Title.t

  let handle ?(tags = Logs.Tag.empty) key_list template_list filter query title =
    Logs.info ~src (fun m -> m "Handle command Update" ~tags);
    let open CCResult in
    let* query = validate_query key_list template_list query in
    Ok
      Filter.
        [ Updated { filter with query; title = Some title } |> Pool_event.filter
        ]
  ;;

  let effects = Filter.Guard.Access.update
end
