open Ppx_yojson_conv_lib.Yojson_conv

let model = Pool_message.Field.Mailing

include Changelog.DefaultSettings

module Id = struct
  include Pool_common.Id

  let to_common m = m
end

module StartAt = struct
  include Pool_model.Base.Ptime

  let field = Pool_message.Field.Start
  let create m = Ok m
  let schema = schema field create
end

module StartNow = struct
  include Pool_model.Base.Boolean

  let schema = schema Pool_message.Field.StartNow
end

module Start = struct
  type t =
    | StartNow
    | StartAt of StartAt.t

  let validate ?(allow_start_in_past = false) start end_at =
    let open CCResult in
    let* start_at =
      match start with
      | StartAt start_at when allow_start_in_past -> Ok start_at
      | StartAt start_at ->
        if Ptime.is_earlier ~than:(Ptime_clock.now ()) start_at
        then Error Pool_message.Error.TimeInPast
        else Ok start_at
      | StartNow -> Ok (Ptime_clock.now ())
    in
    let* () = Pool_model.Time.start_is_before_end ~start:start_at ~end_at in
    Ok start_at
  ;;

  let create start_at start_now =
    match StartNow.value start_now, start_at with
    | true, _ -> Ok StartNow
    | false, Some start_at -> Ok (StartAt start_at)
    | false, None ->
      Error Pool_message.(Error.Conformist [ Field.Start, Error.NoValue ])
  ;;
end

module EndAt = struct
  include Pool_model.Base.Ptime

  let field = Pool_message.Field.End
  let create m = Ok m
  let schema = schema field create
end

module Limit = struct
  include Pool_model.Base.Integer

  let field = Pool_message.Field.Limit

  let create m =
    if m > 0 then Ok m else Error Pool_message.(Error.Invalid field)
  ;;

  let of_int m = m
  let default = 1
  let schema = schema field create
end

module InvitationCount = struct
  include Pool_model.Base.Integer

  let field = Pool_message.Field.InvitationCount

  let create m =
    if m >= 0 then Ok m else Error Pool_message.(Error.Invalid field)
  ;;

  let default = 0
  let schema = schema field create
end

module Distribution = struct
  let print = Utils.ppx_printer

  module SortableField = struct
    module Core = struct
      let field = Pool_message.Field.Distribution

      type t =
        | AssignmentCount [@name "assignment_count"]
        [@printer print "assignment_count"]
        | Firstname [@name "firstname"] [@printer print "firstname"]
        | InvitationCount [@name "count"] [@printer print "count"]
        | Lastname [@name "lastname"] [@printer print "lastname"]
      [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
    end

    include Pool_model.Base.SelectorType (Core)
    include Core

    let to_human language =
      let go = Pool_common.Utils.field_to_string language in
      let open Pool_message in
      function
      | AssignmentCount -> Field.AssignmentCount |> go
      | Firstname -> Field.Firstname |> go
      | InvitationCount -> Field.InvitationCount |> go
      | Lastname -> Field.Lastname |> go
    ;;

    let to_sql = function
      | AssignmentCount -> "pool_contacts.num_assignments"
      | Firstname -> "user_users.given_name"
      | InvitationCount -> "pool_contacts.num_invitations"
      | Lastname -> "user_users.name"
    ;;
  end

  module SortOrder = struct
    include Pool_common.SortOrder

    let to_human lang =
      let open CCFun in
      (function
        | Ascending -> Pool_message.Control.Ascending
        | Descending -> Pool_message.Control.Descending)
      %> Pool_common.Utils.control_to_string lang
    ;;
  end

  type sorted = (SortableField.t * SortOrder.t) list
  [@@deriving eq, show, yojson]

  type t =
    | Sorted of sorted
    | Random
  [@@deriving eq, show { with_path = false }, yojson]

  let field = Pool_message.Field.Distribution
  let create_sorted m = Sorted m
  let value m = m

  let is_random = function
    | Random -> true
    | Sorted _ -> false
  ;;

  let find_dist = function
    | Random -> []
    | Sorted dist -> dist
  ;;

  let get_order_element =
    let open CCFun in
    (function
      | Random -> Some "RAND()"
      | Sorted [] -> None
      | Sorted distribution ->
        distribution
        |> CCList.map (fun (field, order) ->
          CCString.concat
            " "
            [ field |> SortableField.to_sql; order |> SortOrder.show ])
        |> CCString.concat ", "
        |> CCOption.pure)
    %> CCOption.map_or ~default:"" (Format.asprintf "ORDER BY %s")
  ;;

  let is_random_schema () =
    let default = false in
    Pool_conformist.schema_decoder
      ~default
      CCFun.(bool_of_string_opt %> CCOption.get_or ~default %> CCResult.return)
      string_of_bool
      Pool_message.Field.RandomOrder
  ;;

  let schema () =
    let encode m = m |> yojson_of_sorted |> Yojson.Safe.to_string in
    let decode m =
      try
        m |> Yojson.Safe.from_string |> sorted_of_yojson |> CCResult.return
      with
      | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, yojson) ->
        Pool_message.handle_ppx_yojson_err (exn, yojson)
      | _ -> Error Pool_message.(Error.Invalid field)
    in
    Pool_conformist.schema_decoder decode encode field
  ;;

  let of_urlencoded_list =
    let open CCResult in
    function
    | [] -> Ok None
    | data ->
      data
      |> CCList.map (fun distribution_field ->
        match CCString.split ~by:"," distribution_field with
        | [ field; order ] ->
          Ok (Format.asprintf "[[\"%s\"],[\"%s\"]]" field order)
        | _ -> Error Pool_message.(Error.Invalid Field.Distribution))
      |> CCResult.flatten_l
      >|= CCString.concat ","
      >|= Format.asprintf "[%s]"
      >|= CCOption.return
  ;;
end

type t =
  { id : Id.t
  ; start_at : StartAt.t
  ; end_at : EndAt.t
  ; limit : Limit.t
  ; distribution : Distribution.t option
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show, yojson]

let create
  ?allow_start_in_past
  ?(id = Id.create ())
  start
  end_at
  limit
  distribution
  =
  let open CCResult in
  let* start_at = Start.validate ?allow_start_in_past start end_at in
  Ok
    { id
    ; start_at
    ; end_at
    ; limit
    ; distribution
    ; created_at = Pool_common.CreatedAt.create_now ()
    ; updated_at = Pool_common.UpdatedAt.create_now ()
    }
;;

let seconds_per_minute = 60
let seconds_per_hour = 60 * seconds_per_minute

let per_interval interval { start_at; end_at; limit; _ } =
  let open Ptime in
  let open CCFloat in
  let limit = of_int limit in
  let duration = diff end_at start_at |> Span.to_float_s |> max 0. in
  if limit <= 0.
  then 0.
  else limit / duration * Span.to_float_s interval |> max 0. |> min limit
;;

let is_past { end_at; _ } = Ptime.is_later ~than:end_at (Ptime_clock.now ())

open Pool_message
open Query

let column_start = (Field.start, "pool_mailing.`start`") |> Column.create
let column_end = (Field.End, "pool_mailing.`end`") |> Column.create
let column_limit = (Field.Limit, "pool_mailing.`limit`") |> Column.create

let column_invitation_count =
  (Field.InvitationCount, "invitation_count") |> Column.create
;;

let column_past = (Field.HidePast, "pool_mailing.end >= NOW()") |> Column.create
let filterable_by = Some Filter.Condition.Human.[ Checkbox column_past ]
let searchable_by = []

let sortable_by =
  searchable_by
  @ [ column_start; column_end; column_limit; column_invitation_count ]
;;

let default_sort =
  let open Query in
  Sort.{ column = column_start; order = SortOrder.Ascending }
;;

let default_filter =
  let open Filter in
  Condition.[ Checkbox (column_past, true) ]
;;

let default_query = create ~sort:default_sort ~filter:default_filter ()
