module Conformist = Pool_common.Utils.PoolConformist
module Id = Pool_common.Id

module Create : sig
  type t = Filter.filter

  val handle
    :  Filter.Key.human list
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Filter.t -> Guard.Authorizer.effect list
end = struct
  type t = Filter.filter

  let handle key_list predicate =
    let open CCResult in
    let* predicate = Filter.validate_filter key_list predicate in
    Ok [ Filter.Created (Filter.create predicate) |> Pool_event.filter ]
  ;;

  let effects filter =
    [ `Update, `One (filter.Filter.id |> Pool_common.Id.to_uuidm) ]
  ;;
end

module Update : sig
  type t = Filter.filter

  val handle
    :  Filter.Key.human list
    -> Filter.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Filter.t -> Guard.Authorizer.effect list
end = struct
  type t = Filter.filter

  let handle key_list filter predicate =
    let open CCResult in
    let* predicate = Filter.validate_filter key_list predicate in
    Ok
      Filter.[ Updated { filter with filter = predicate } |> Pool_event.filter ]
  ;;

  let effects filter =
    [ `Update, `One (filter.Filter.id |> Pool_common.Id.to_uuidm) ]
  ;;
end
