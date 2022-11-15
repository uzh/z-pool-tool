module Conformist = Pool_common.Utils.PoolConformist
module Id = Pool_common.Id

let default_schema command =
  Pool_common.Utils.PoolConformist.(
    make Field.[ Filter.Title.schema () ] command)
;;

let default_command = CCFun.id

module Create : sig
  type t = Filter.Title.t

  val handle
    :  Filter.Key.human list
    -> Filter.t list
    -> Filter.filter
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects : Filter.t -> Guard.Authorizer.effect list
end = struct
  type t = Filter.Title.t

  let handle key_list subfilter_list predicate title =
    let open CCResult in
    let* predicate = Filter.validate_filter key_list subfilter_list predicate in
    Ok
      [ Filter.Created (Filter.create (Some title) predicate)
        |> Pool_event.filter
      ]
  ;;

  let decode data =
    Conformist.decode_and_validate (default_schema default_command) data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects filter =
    [ `Update, `One (filter.Filter.id |> Pool_common.Id.to_uuidm) ]
  ;;
end

module Update : sig
  type t = Filter.Title.t

  val handle
    :  Filter.Key.human list
    -> Filter.t list
    -> Filter.t
    -> Filter.filter
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects : Filter.t -> Guard.Authorizer.effect list
end = struct
  type t = Filter.Title.t

  let handle key_list subfilter_list filter predicate title =
    let open CCResult in
    let* predicate = Filter.validate_filter key_list subfilter_list predicate in
    Ok
      Filter.
        [ Updated { filter with filter = predicate; title = Some title }
          |> Pool_event.filter
        ]
  ;;

  let decode data =
    Conformist.decode_and_validate (default_schema default_command) data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects filter =
    [ `Update, `One (filter.Filter.id |> Pool_common.Id.to_uuidm) ]
  ;;
end
