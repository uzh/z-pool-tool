open Ppx_yojson_conv_lib.Yojson_conv

module type IdSig = sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val create : unit -> t
  val of_string : string -> t
  val value : t -> string
  val to_common : t -> t
  val of_common : t -> t
  val compare : t -> t -> int
  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
  val t_of_sexp : Sexplib0.Sexp.t -> t

  val schema
    :  ?field:Entity_message.Field.t
    -> unit
    -> (Entity_message.error, t) Pool_common_utils.PoolConformist.Field.t

  val sql_select_fragment : field:string -> string
  val sql_value_fragment : string -> string
end

module Boolean = struct
  open Sexplib.Conv

  type t = bool [@@deriving eq, ord, show, sexp_of, yojson]

  let create m = m
  let value m = m

  let stringify = function
    | true -> "true"
    | false -> "false"
  ;;

  let of_string = function
    | "true" -> true
    | _ -> false
  ;;

  let schema field ()
    : (Entity_message.error, t) Pool_common_utils.PoolConformist.Field.t
    =
    Pool_common_utils.schema_decoder
      (fun m ->
        m
        |> bool_of_string_opt
        |> CCOption.get_or ~default:false
        |> CCResult.return)
      string_of_bool
      field
  ;;
end

module type BooleanSig = sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val create : bool -> t
  val value : t -> bool
  val stringify : t -> string
  val of_string : string -> t
  val compare : t -> t -> int

  val schema
    :  unit
    -> (Entity_message.error, t) Pool_common_utils.PoolConformist.Field.t
end

module String = struct
  open Sexplib.Conv

  type t = string [@@deriving eq, show, sexp_of, yojson]

  let compare = CCString.compare
  let value m = m
  let of_string m = m

  let create str =
    if CCString.is_empty str then Error Entity_message.NoValue else Ok str
  ;;

  let schema field ?validation ()
    : (Entity_message.error, t) Pool_common_utils.PoolConformist.Field.t
    =
    let create = CCOption.value ~default:create validation in
    Pool_common_utils.schema_decoder create value field
  ;;
end

module type StringSig = sig
  type t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val create : string -> (t, Entity_message.error) result
  val value : t -> string
  val of_string : string -> t

  val schema
    :  unit
    -> (Entity_message.error, t) Pool_common_utils.PoolConformist.Field.t
end

module Integer = struct
  open Sexplib.Conv

  type t = int [@@deriving eq, ord, show, sexp_of, yojson]

  let value m = m
  let to_string t = Int.to_string t

  let schema field create ()
    : (Entity_message.error, t) Pool_common_utils.PoolConformist.Field.t
    =
    let decode str =
      let open CCResult in
      CCInt.of_string str
      |> CCOption.to_result Entity_message.(NotANumber str)
      >>= create
    in
    Pool_common_utils.schema_decoder decode CCInt.to_string field
  ;;
end

module type IntegerSig = sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val create : int -> (t, Entity_message.error) result
  val value : t -> int
  val compare : t -> t -> int

  val schema
    :  unit
    -> (Entity_message.error, t) Pool_common_utils.PoolConformist.Field.t
end

module PtimeSpan = struct
  type t = Ptime.Span.t [@@deriving eq, show]

  let sexp_of_t = Pool_common_utils.Time.ptime_span_to_sexp
  let t_of_yojson = Utils_time.ptime_span_of_yojson
  let yojson_of_t = Utils_time.yojson_of_ptime_span
  let value m = m
  let of_span m = m
  let to_human = Pool_common_utils.Time.formatted_timespan
  let to_int_s = Ptime.Span.to_int_s
  let of_int_s = Ptime.Span.of_int_s
  let abs = Ptime.Span.abs
  let compare = Ptime.Span.compare

  let schema field create ()
    : (Entity_message.error, t) Pool_common_utils.PoolConformist.Field.t
    =
    let open Pool_common_utils in
    let open CCResult in
    let decode str = Time.parse_time_span str >>= create in
    let encode = Time.print_time_span in
    schema_decoder decode encode field
  ;;
end

module type PtimeSpanSig = sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val value : t -> Ptime.Span.t
  val to_int_s : t -> int option
  val of_int_s : int -> t
  val abs : t -> t
  val of_span : Ptime.Span.t -> t
  val to_human : t -> string

  val schema
    :  unit
    -> (Entity_message.error, t) Pool_common_utils.PoolConformist.Field.t
end

module Ptime = struct
  type t = Ptime.t [@@deriving eq, show]
  type date = Ptime.date

  let sexp_of_t = Pool_common_utils.Time.ptime_to_sexp
  let t_of_yojson = Utils.Ptime.ptime_of_yojson
  let yojson_of_t = Utils.Ptime.yojson_of_ptime
  let value m = m
  let create_now = Ptime_clock.now
  let to_human = Utils.Ptime.formatted_date_time
  let date_time_to_flatpickr = Ptime.to_rfc3339
  let compare = Ptime.compare

  (* Date *)
  let equal_date (y1, m1, d1) (y2, m2, d2) =
    CCInt.(equal y1 y2 && equal m1 m2 && equal d1 d2)
  ;;

  let date_of_string = Utils_time.parse_date

  let date_to_string (y, m, d) =
    let decimal = Utils.Ptime.decimal in
    Format.asprintf "%s-%s-%s" (decimal y) (decimal m) (decimal d)
  ;;

  let date_to_flatpickr date =
    date
    |> Ptime.of_date
    |> CCOption.get_exn_or "Invalid date provided"
    |> date_time_to_flatpickr
  ;;

  let pp_date formatter t = CCString.pp formatter (date_to_string t)

  let schema field create ()
    : (Entity_message.error, t) Pool_common_utils.PoolConformist.Field.t
    =
    let decode str =
      let open CCResult in
      Pool_common_utils.Time.parse_time str >>= create
    in
    Pool_common_utils.schema_decoder decode Ptime.to_rfc3339 field
  ;;
end

module type PtimeSig = sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val value : t -> Ptime.t
  val create_now : unit -> t
  val to_human : t -> string
  val compare : t -> t -> int

  val schema
    :  unit
    -> (Entity_message.error, t) Pool_common_utils.PoolConformist.Field.t
end

module type BaseSig = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> (t, Entity_message.error) result

  val schema
    :  unit
    -> (Entity_message.error, t) Pool_common_utils.PoolConformist.Field.t
end

module type SelectorCoreTypeSig = sig
  type t

  val field : Entity_message.Field.t
  val min : int
  val max : int
  val to_enum : t -> int
  val of_enum : int -> t option
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
end

module SelectorType (Core : SelectorCoreTypeSig) = struct
  open CCFun
  include Core

  let to_yojson_string m = m |> Format.asprintf "[\"%s\"]"
  let read = to_yojson_string %> Yojson.Safe.from_string %> Core.t_of_yojson

  let create m =
    try Ok (read m) with
    | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, yojson) ->
      Pool_common_utils.handle_ppx_yojson_err (exn, yojson)
    | _ -> Error Entity_message.(Invalid field)
  ;;

  let all : t list =
    CCList.range min max
    |> CCList.map of_enum
    |> CCList.all_some
    |> CCOption.get_exn_or
         (Format.asprintf
            "%s: Could not create list of all keys!"
            ([%show: Entity_message.Field.t] field))
  ;;

  let schema () = Pool_common_utils.schema_decoder create show field
end

module TimeUnit = struct
  let print = Utils.ppx_printer

  module Core = struct
    let field = Entity_message_field.TimeUnit

    type t =
      | Seconds [@name "seconds"] [@printer print "seconds"]
      | Minutes [@name "minutes"] [@printer print "minutes"]
      | Hours [@name "hours"] [@printer print "hours"]
      | Days [@name "days"] [@printer print "days"]
    [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]

    let factor = function
      | Seconds -> 1
      | Minutes -> 60
      | Hours -> 60 * 60
      | Days -> 60 * 60 * 24
    ;;
  end

  include SelectorType (Core)
  include Core

  let default_unit = Minutes

  let read m =
    m |> Format.asprintf "[\"%s\"]" |> Yojson.Safe.from_string |> t_of_yojson
  ;;

  let of_string str =
    try Ok (read str) with
    | _ -> Error Entity_message.(Invalid Core.field)
  ;;

  let named_field name = Entity_message_field.TimeUnitOf name

  let named_schema name () =
    Pool_common_utils.schema_decoder of_string show (named_field name)
  ;;

  let to_human = CCFun.(show %> CCString.capitalize_ascii)
  let to_seconds value unit = value * factor unit

  let ptime_span_to_largest_unit span =
    let seconds = PtimeSpan.to_int_s span |> CCOption.value ~default:0 in
    let default = Seconds, seconds in
    let rec folder = function
      | [] -> default
      | hd :: tl ->
        (match seconds mod factor hd with
         | 0 -> hd, seconds / factor hd
         | _ -> folder tl)
    in
    if seconds > 0 then all |> CCList.rev |> folder else default
  ;;
end

module type DurationCore = sig
  val name : Entity_message_field.t
end

module Duration (Core : DurationCore) = struct
  include PtimeSpan
  include Core

  let value = PtimeSpan.value

  let to_ptime_span value unit =
    TimeUnit.to_seconds value unit |> PtimeSpan.of_int_s
  ;;

  let create m =
    if PtimeSpan.(equal (abs m) m)
    then Ok m
    else Error Entity_message.NegativeAmount
  ;;

  let of_int_s s =
    if s >= 0
    then Ok (s |> PtimeSpan.of_int_s)
    else Error Entity_message.NegativeAmount
  ;;

  let of_int value unit = to_ptime_span value unit |> create

  let of_int_opt value unit =
    match value, unit with
    | Some value, Some unit -> of_int value unit |> CCResult.map CCOption.return
    | _, _ -> Ok None
  ;;

  let with_largest_unit (m : t) = TimeUnit.ptime_span_to_largest_unit m
  let integer_schema = Integer.schema name CCResult.return
end

module type DurationSig = sig
  type t

  include DurationCore

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val value : t -> PtimeSpan.t
  val create : PtimeSpan.t -> (t, Entity_message.error) result
  val of_int_s : int -> (t, Entity_message.error) result
  val of_int : int -> TimeUnit.t -> (t, Entity_message.error) result
  val to_int_s : t -> int option

  val of_int_opt
    :  int option
    -> TimeUnit.t option
    -> (t option, Entity_message.error) result

  val to_human : t -> string
  val to_ptime_span : int -> TimeUnit.t -> PtimeSpan.t
  val with_largest_unit : t -> TimeUnit.t * int

  val integer_schema
    :  unit
    -> (Entity_message.error, int) Pool_common_utils.PoolConformist.Field.t
end
