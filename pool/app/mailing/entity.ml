module Id = struct
  include Pool_common.Id

  let to_common m = m
end

module StartAt = struct
  include Pool_common.Model.Ptime

  let field = Pool_common.Message.Field.Start
  let create m = Ok m
  let schema = schema field create
end

module StartNow = struct
  include Pool_common.Model.Boolean

  let schema = schema Pool_common.Message.Field.StartNow
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
        then Error Pool_common.Message.TimeInPast
        else Ok start_at
      | StartNow -> Ok (Ptime_clock.now ())
    in
    if Ptime.is_later ~than:end_at start_at
    then Error Pool_common.Message.EndBeforeStart
    else Ok start_at
  ;;

  let create start_at start_now =
    match StartNow.value start_now, start_at with
    | true, _ -> Ok StartNow
    | false, Some start_at -> Ok (StartAt start_at)
    | false, None ->
      Error Pool_common.Message.(Conformist [ Field.Start, NoValue ])
  ;;
end

module EndAt = struct
  include Pool_common.Model.Ptime

  let field = Pool_common.Message.Field.End
  let create m = Ok m
  let schema = schema field create
end

module Rate = struct
  include Pool_common.Model.Integer

  let field = Pool_common.Message.Field.Rate

  let create m =
    if m > 0 then Ok m else Error Pool_common.Message.(Invalid field)
  ;;

  let default = 1
  let schema = schema field create
end

module Distribution = struct
  let print = Utils.ppx_printer

  module SortableField = struct
    module Core = struct
      let field = Pool_common.Message.Field.Distribution

      type t =
        | AssignmentCount [@name "assignment_count"]
            [@printer print "assignment_count"]
        | Firstname [@name "firstname"] [@printer print "firstname"]
        | InvitationCount [@name "invitation_count"]
            [@printer print "invitation_count"]
        | Lastname [@name "lastname"] [@printer print "lastname"]
      [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
    end

    include Pool_common.Model.SelectorType (Core)
    include Core

    let to_human language =
      let go = Pool_common.Utils.field_to_string language in
      let open Pool_common.Message in
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
      let open Pool_common in
      (function
       | Ascending -> Message.Ascending
       | Descending -> Message.Descending)
      %> Utils.control_to_string lang
    ;;
  end

  type sorted = (SortableField.t * SortOrder.t) list
  [@@deriving eq, show, yojson]

  type t =
    | Sorted of sorted
    | Random
  [@@deriving eq, show { with_path = false }, yojson]

  let field = Pool_common.Message.Field.Distribution
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
    let open Pool_common in
    let default = false in
    Utils.schema_decoder
      ~default
      CCFun.(bool_of_string_opt %> CCOption.get_or ~default %> CCResult.return)
      string_of_bool
      Pool_common.Message.Field.RandomOrder
  ;;

  let schema () =
    let encode m = m |> yojson_of_sorted |> Yojson.Safe.to_string in
    let decode m =
      try
        m |> Yojson.Safe.from_string |> sorted_of_yojson |> CCResult.return
      with
      | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, yojson) ->
        Pool_common.Utils.handle_ppx_yojson_err (exn, yojson)
      | _ -> Error Pool_common.Message.(Invalid field)
    in
    Pool_common.Utils.schema_decoder decode encode field
  ;;

  let of_urlencoded_list data =
    let open CCResult in
    data
    |> CCList.map (fun distribution_field ->
         match CCString.split ~by:"," distribution_field with
         | [ field; order ] ->
           Ok (Format.asprintf "[[\"%s\"],[\"%s\"]]" field order)
         | _ -> Error Pool_common.Message.(Invalid Field.Distribution))
    |> CCResult.flatten_l
    >|= CCString.concat ","
    >|= Format.asprintf "[%s]"
  ;;
end

type t =
  { id : Id.t
  ; start_at : StartAt.t
  ; end_at : EndAt.t
  ; rate : Rate.t
  ; distribution : Distribution.t option
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving show]

let equal m1 m2 =
  Id.equal m1.id m2.id
  && StartAt.equal m1.start_at m2.start_at
  && EndAt.equal m1.end_at m2.end_at
  && Rate.equal m1.rate m2.rate
  && CCOption.map2 Distribution.equal m1.distribution m2.distribution
     |> CCOption.get_or ~default:false
;;

let create
  ?allow_start_in_past
  ?(id = Id.create ())
  start
  end_at
  rate
  distribution
  =
  let open CCResult in
  let* start_at = Start.validate ?allow_start_in_past start end_at in
  Ok
    { id
    ; start_at
    ; end_at
    ; rate
    ; distribution
    ; created_at = Pool_common.CreatedAt.create ()
    ; updated_at = Pool_common.UpdatedAt.create ()
    }
;;

let seconds_per_minute = 60
let seconds_per_hour = 60 * seconds_per_minute

let per_minutes n_minutes { rate; _ } =
  let open CCFloat in
  let total_seconds = CCInt.(n_minutes * seconds_per_minute |> to_float) in
  of_int rate / of_int seconds_per_hour * total_seconds
;;

let total { start_at; end_at; rate; _ } =
  let open CCFloat in
  of_int rate
  / of_int seconds_per_hour
  * (Ptime.diff end_at start_at |> Ptime.Span.to_float_s)
  |> round
  |> to_int
;;
