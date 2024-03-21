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
  include Common.CommandSig with type t = Filter.t

  val create_filter
    :  ?id:Filter.Id.t
    -> Filter.Key.human list
    -> t list
    -> Filter.query
    -> Filter.Title.t
    -> (t, Conformist.error_msg) result

  val handle
    :  ?tags:Logs.Tag.set
    -> (Assignment.t * Assignment.MatchesFilter.t) list
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result
end = struct
  type t = Filter.t

  let create_filter
    ?(id = Filter.Id.create ())
    key_list
    template_list
    query
    title
    =
    let open CCResult in
    let* query = validate_query key_list template_list query in
    Filter.create ~id (Some title) query |> return
  ;;

  let handle ?(tags = Logs.Tag.empty) assignments filter =
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
    let open CCResult in
    let assignment_events =
      assignments
      |> Assignment.update_matches_filter_events
      |> CCList.map Pool_event.assignment
    in
    Ok ((Filter.Created filter |> Pool_event.filter) :: assignment_events)
  ;;

  let effects = Filter.Guard.Access.create
end

module Update : sig
  include Common.CommandSig with type t = Filter.t

  val create_filter
    :  Filter.Key.human list
    -> Filter.t list
    -> Filter.t
    -> Filter.query
    -> Filter.Title.t
    -> (Filter.t, Pool_common.Message.error) result

  val handle
    :  ?tags:Logs.Tag.set
    -> (Assignment.t * Assignment.MatchesFilter.t) list
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Filter.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Filter.t

  let create_filter key_list template_list filter query title =
    let open CCResult in
    let* query = validate_query key_list template_list query in
    Ok Filter.{ filter with query; title = Some title }
  ;;

  let handle ?(tags = Logs.Tag.empty) assignments filter =
    Logs.info ~src (fun m -> m "Handle command Update" ~tags);
    let open CCResult in
    let assignment_events =
      assignments
      |> Assignment.update_matches_filter_events
      |> CCList.map Pool_event.assignment
    in
    Ok ((Filter.Updated filter |> Pool_event.filter) :: assignment_events)
  ;;

  let effects = Filter.Guard.Access.update
end
