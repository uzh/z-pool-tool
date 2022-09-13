module Id = struct
  include Pool_common.Id
end

module StartAt = struct
  type t = Ptime.t [@@deriving eq, show]

  let field = Pool_common.Message.Field.Start
  let create m = Ok m
  let value m = m
  let to_human = Pool_common.Utils.Time.formatted_date_time

  let schema ?(field = field) () =
    let decode str =
      let open CCResult in
      Pool_common.(Utils.Time.parse_time str >>= create)
    in
    Pool_common.Utils.schema_decoder decode Ptime.to_rfc3339 field
  ;;
end

module EndAt = struct
  include StartAt

  let field = Pool_common.Message.Field.End
  let schema = schema ~field
end

module Rate = struct
  type t = int [@@deriving eq, show]

  let field = Pool_common.Message.Field.Rate

  let create m =
    if m > 0 then Ok m else Error Pool_common.Message.(Invalid Field.Rate)
  ;;

  let value m = m
  let default = 1

  let schema () =
    let decode str =
      let open CCResult in
      CCInt.of_string str
      |> CCOption.to_result Pool_common.Message.(NotANumber str)
      >>= create
    in
    Pool_common.Utils.schema_decoder decode CCInt.to_string field
  ;;
end

module Distribution = struct
  let sortable_fields =
    Pool_common.Message.Field.
      [ AssignmentCount; InvitationCount; Firstname; Name ]
  ;;

  module SortOrder = struct
    let field = Pool_common.Message.Field.SortOrder
    let go m fmt _ = Format.pp_print_string fmt m

    type t =
      | Ascending [@name "ASC"] [@printer go "ASC"]
      | Descending [@name "DESC"] [@printer go "DESC"]
    [@@deriving eq, show, yojson, enum]

    let to_human m lang =
      let open Pool_common in
      Utils.control_to_string
        lang
        (match m with
         | Ascending -> Message.Ascending
         | Descending -> Message.Descending)
    ;;

    let read m =
      m |> Format.asprintf "[\"%s\"]" |> Yojson.Safe.from_string |> t_of_yojson
    ;;

    let create m =
      try Ok (read m) with
      | _ -> Error Pool_common.Message.(Invalid field)
    ;;

    let all : t list =
      CCList.range min max
      |> CCList.map of_enum
      |> CCList.all_some
      |> CCOption.get_exn_or
           "Distribution order: Could not create list of all keys!"
    ;;

    let default = Ascending
    let label m = m |> show |> Utils.Countries.find
    let schema () = Pool_common.Utils.schema_decoder create show field
  end

  type t = (Pool_common.Message.Field.t * SortOrder.t) list
  [@@deriving eq, show, yojson]

  let field = Pool_common.Message.Field.Distribution
  let create m = m
  let value m = m

  let get_order_element m =
    m
    |> CCList.map (fun (field, order) ->
         CCString.concat
           " "
           [ field |> Pool_common.Message.Field.show; order |> SortOrder.show ])
    |> CCString.concat ", "
    |> Format.asprintf "ORDER BY %s"
  ;;

  let schema () =
    let encode m = m |> yojson_of_t |> Yojson.Safe.to_string in
    let decode =
      CCResult.(fun m -> m |> Yojson.Safe.from_string |> t_of_yojson |> pure)
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

let create ?(id = Id.create ()) start_at end_at rate distribution =
  if Ptime.is_later ~than:end_at start_at
  then Error Pool_common.Message.EndBeforeStart
  else if Ptime.is_earlier ~than:(Ptime_clock.now ()) start_at
  then Error Pool_common.Message.TimeInPast
  else
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
