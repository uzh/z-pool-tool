module Id = struct
  include Pool_common.Id

  let to_common m = m
end

module NoShow = struct
  include Pool_common.Model.Boolean

  let init = false
  let schema = schema Pool_common.Message.Field.NoShow
end

module Participated = struct
  include Pool_common.Model.Boolean

  let init = false
  let schema = schema Pool_common.Message.Field.Participated
end

module MatchesFilter = struct
  type t = bool [@@deriving eq, show]

  let create m = m
  let value m = m
  let init = false
end

module CanceledAt = struct
  include Pool_common.Model.Ptime

  let create m = Ok m
  let schema = schema Pool_common.Message.Field.CanceledAt create
end

module MarkedAsDeleted = struct
  include Pool_common.Model.Boolean

  let init = false
  let schema = schema Pool_common.Message.Field.MarkedAsDeleted
end

module ExternalDataId = struct
  include Pool_common.Model.String

  let field = Pool_common.Message.Field.ExternalDataId
  let schema () = schema field ()
end

type t =
  { id : Id.t [@equal fun a b -> Id.equal a b || Sihl.Configuration.is_test ()]
  ; contact : Contact.t
  ; no_show : NoShow.t option
  ; participated : Participated.t option
  ; matches_filter : MatchesFilter.t
  ; canceled_at : CanceledAt.t option
  ; marked_as_deleted : MarkedAsDeleted.t
  ; external_data_id : ExternalDataId.t option
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show]

let create
  ?(id = Id.create ())
  ?no_show
  ?participated
  ?(matches_filter = MatchesFilter.create true)
  ?canceled_at
  ?(marked_as_deleted = MarkedAsDeleted.init)
  ?external_data_id
  contact
  =
  { id
  ; contact
  ; no_show
  ; participated
  ; matches_filter
  ; canceled_at
  ; marked_as_deleted
  ; external_data_id
  ; created_at = Pool_common.CreatedAt.create ()
  ; updated_at = Pool_common.UpdatedAt.create ()
  }
;;

let is_not_deleted { marked_as_deleted; _ } =
  if not marked_as_deleted
  then Ok ()
  else Error Pool_common.Message.(IsMarkedAsDeleted Field.Assignment)
;;

let is_not_closed { no_show; participated; _ } =
  if CCOption.(is_none no_show && is_none participated)
  then Ok ()
  else Error Pool_common.Message.AssignmentIsClosed
;;

let is_not_canceled { canceled_at; _ } =
  if CCOption.is_none canceled_at
  then Ok ()
  else Error Pool_common.Message.AssignmentIsCanceled
;;

let is_deletable m =
  let open CCResult in
  let* () = is_not_deleted m in
  Ok ()
;;

let is_cancellable m =
  let open CCResult in
  let* () = is_deletable m in
  let* () = is_not_canceled m in
  Ok ()
;;

let attendance_settable m =
  let open CCResult in
  let* () = is_not_deleted m in
  let* () = is_not_canceled m in
  Ok ()
;;

module Public = struct
  type t =
    { id : Id.t
    ; canceled_at : CanceledAt.t option
    }
end

module IncrementParticipationCount = struct
  type t = bool

  let value m = m
  let create m = m
end

let validate experiment { no_show; participated; external_data_id; _ } =
  let value = CCOption.value ~default:false in
  let open Pool_common.Message in
  [ ( Experiment.external_data_required_value experiment
      && CCOption.is_none external_data_id
    , Missing Field.ExternalDataId )
  ; ( value no_show && value participated
    , MutuallyExclusive (Field.NoShow, Field.Participated) )
  ]
  |> CCList.filter_map (fun (condition, error) ->
    if condition then Some error else None)
  |> function
  | [] -> None
  | errors -> Some errors
;;
