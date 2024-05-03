module Id = struct
  include Pool_common.Id

  let to_common m = m
end

module NoShow = struct
  include Pool_model.Base.Boolean

  let field = Pool_message.Field.NoShow
  let init = false
  let schema = schema field
end

module Participated = struct
  include Pool_model.Base.Boolean

  let field = Pool_message.Field.Participated
  let init = false
  let schema = schema field
end

module MatchesFilter = struct
  type t = bool [@@deriving eq, show]

  let create m = m
  let value m = m
  let init = false
end

module CanceledAt = struct
  include Pool_model.Base.Ptime

  let create m = Ok m
  let schema = schema Pool_message.Field.CanceledAt create
end

module MarkedAsDeleted = struct
  include Pool_model.Base.Boolean

  let init = false
  let schema = schema Pool_message.Field.MarkedAsDeleted
end

module ExternalDataId = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.ExternalDataId
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
  ; reminder_manually_last_sent_at : Pool_common.Reminder.SentAt.t option
  ; custom_fields : Custom_field.Public.t list option
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
  ?reminder_manually_last_sent_at
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
  ; reminder_manually_last_sent_at
  ; custom_fields = None
  ; created_at = Pool_common.CreatedAt.create_now ()
  ; updated_at = Pool_common.UpdatedAt.create_now ()
  }
;;

module ExternalDataIdentifier = struct
  type t =
    { external_data_id : ExternalDataId.t
    ; experiment_id : Experiment.Id.t
    ; experiment_title : Experiment.Title.t
    ; session_id : Session.Id.t
    ; session_start : Session.Start.t
    ; session_duration : Session.Duration.t
    }
end

let is_not_deleted { marked_as_deleted; _ } =
  if not marked_as_deleted
  then Ok ()
  else Error Pool_message.(Error.IsMarkedAsDeleted Field.Assignment)
;;

let is_not_canceled { canceled_at; _ } =
  if CCOption.is_none canceled_at
  then Ok ()
  else Error Pool_message.Error.AssignmentIsCanceled
;;

let is_not_canceled_nor_deleted m =
  let open CCResult in
  let* () = is_not_deleted m in
  let* () = is_not_canceled m in
  Ok ()
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

let attendance_settable = is_not_canceled_nor_deleted

let session_changeable current_session m =
  let open CCResult in
  let* () = is_not_canceled_nor_deleted m in
  let* () = Session.assignments_session_changeable current_session in
  Ok ()
;;

let reminder_sendable session m =
  let open CCResult in
  let* () = is_not_canceled_nor_deleted m in
  let* () = Session.reminder_resendable session in
  Ok ()
;;

module Public = struct
  type t =
    { id : Id.t
    ; participated : Participated.t option
    ; canceled_at : CanceledAt.t option
    ; created_at : Pool_common.CreatedAt.t
    ; updated_at : Pool_common.UpdatedAt.t
    }
  [@@deriving eq, show]

  let participated ({ participated; _ } : t) = participated
end

module IncrementParticipationCount = Pool_model.Base.Boolean

let validate experiment { no_show; participated; external_data_id; _ } =
  let value = CCOption.value ~default:false in
  let open Pool_message in
  [ ( Experiment.external_data_required_value experiment
      && CCOption.is_none external_data_id
      && value participated
    , Error.FieldRequired Field.ExternalDataId )
  ; ( value no_show && value participated
    , Error.MutuallyExclusive (Field.NoShow, Field.Participated) )
  ]
  |> CCList.filter_map (fun (condition, error) ->
    if condition then Some error else None)
  |> function
  | [] -> Ok ()
  | errors -> Error errors
;;

let set_close_default_values ({ no_show; participated; _ } as m) =
  let default = CCOption.value ~default:false in
  let no_show = default no_show in
  let participated = default participated in
  ( { m with no_show = Some no_show; participated = Some participated }
  , no_show
  , participated )
;;

let boolean_fields = Pool_message.Field.[ NoShow; Participated ]

open Pool_message

let column_canceled_at =
  (Field.CanceledAt, "pool_assignments.canceled_at") |> Query.Column.create
;;

let column_external_data_id =
  (Field.ExternalDataId, "pool_assignments.external_data_id")
  |> Query.Column.create
;;

let column_external_data_id_abbr =
  (Field.ExternalDataIdAbbr, "pool_assignments.external_data_id")
  |> Query.Column.create
;;

let column_no_show =
  (Field.NoShowAbr, "pool_assignments.no_show") |> Query.Column.create
;;

let column_participated =
  (Field.ParticipatedAbr, "pool_assignments.participated")
  |> Query.Column.create
;;

let column_marked_as_deleted =
  Query.Column.create
    (Field.HideMakedAsDeleted, "pool_assignments.marked_as_deleted = 0")
;;

let column_canceled =
  Query.Column.create
    (Field.HideCanceled, "pool_assignments.canceled_at IS NULL")
;;

let filterable_by =
  Some
    Query.Filter.Condition.Human.
      [ Checkbox column_marked_as_deleted; Checkbox column_canceled ]
;;

let searchable_by = Pool_user.searchable_by @ [ column_external_data_id ]

let sortable_by =
  searchable_by
  @ [ column_canceled_at
    ; column_no_show
    ; column_participated
    ; column_external_data_id
    ; column_external_data_id_abbr
    ; Contact.column_cell_phone
    ]
;;

let default_sort = Pool_user.default_sort

let default_filter =
  Query.Filter.Condition.[ Checkbox (column_marked_as_deleted, true) ]
;;

let default_query = Query.create ~sort:default_sort ~filter:default_filter ()
