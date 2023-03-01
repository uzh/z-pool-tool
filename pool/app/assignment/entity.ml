module Id = struct
  include Pool_common.Id

  let to_common m = m
end

module ShowUp = struct
  include Pool_common.Model.Boolean

  let init = false
  let schema = schema Pool_common.Message.Field.ShowUp
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

type t =
  { id : Pool_common.Id.t
  ; contact : Contact.t
  ; show_up : ShowUp.t option
  ; participated : Participated.t option
  ; matches_filter : MatchesFilter.t
  ; canceled_at : CanceledAt.t option
  ; marked_as_deleted : MarkedAsDeleted.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show]

let create
  ?(id = Pool_common.Id.create ())
  ?show_up
  ?participated
  ?(matches_filter = MatchesFilter.create true)
  ?canceled_at
  ?(marked_as_deleted = MarkedAsDeleted.init)
  contact
  =
  { id
  ; contact
  ; show_up
  ; participated
  ; matches_filter
  ; canceled_at
  ; marked_as_deleted
  ; created_at = Pool_common.CreatedAt.create ()
  ; updated_at = Pool_common.UpdatedAt.create ()
  }
;;

let is_not_deleted { marked_as_deleted; _ } =
  if not marked_as_deleted
  then Ok ()
  else Error Pool_common.Message.(IsMarkedAsDeleted Field.Assignment)
;;

let is_not_canceled { canceled_at; _ } =
  if CCOption.is_none canceled_at
  then Ok ()
  else Error Pool_common.Message.AssignmentIsCanceled
;;

(* TODO: under which circumstances should an assignment be maked_as_deleted *)
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
    { id : Pool_common.Id.t
    ; canceled_at : CanceledAt.t option
    }
end
