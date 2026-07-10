open Ppx_yojson_conv_lib.Yojson_conv

module Id = struct
  open Sexplib.Conv

  type t = string [@@deriving eq, show, sexp, yojson, ord]

  let random_state = Random.State.make_self_init ()
  let create () = Uuidm.v4_gen random_state () |> Uuidm.to_string
  let of_string m = m

  let validate m =
    m
    |> Uuidm.of_string
    |> CCOption.to_result Pool_message.Error.(Invalid Pool_message.Field.Id)
    |> CCResult.map Uuidm.to_string
  ;;

  let value m = m
  let to_common m = m
  let of_common m = m
  let compare = CCString.compare

  let schema ?(field = Pool_message.Field.Id) () =
    Pool_conformist.schema_decoder CCFun.(of_string %> CCResult.return) value field
  ;;

  let sql_select_fragment ~field =
    [%string
      {sql|
        LOWER(CONCAT(
          SUBSTR(HEX(%{field}), 1, 8), '-',
          SUBSTR(HEX(%{field}), 9, 4), '-',
          SUBSTR(HEX(%{field}), 13, 4), '-',
          SUBSTR(HEX(%{field}), 17, 4), '-',
          SUBSTR(HEX(%{field}), 21)
        ))
    |sql}]
  ;;

  let sql_value_fragment name = [%string {sql| UNHEX(REPLACE(%{name}, '-', '')) |sql}]
end

module type IdSig = sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val create : unit -> t
  val of_string : string -> t
  val validate : string -> (t, Pool_message.Error.t) result
  val value : t -> string
  val to_common : t -> t
  val of_common : t -> t
  val compare : t -> t -> int
  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
  val t_of_sexp : Sexplib0.Sexp.t -> t

  val schema
    :  ?field:Pool_message.Field.t
    -> unit
    -> (Pool_message.Error.t, t) Pool_conformist.Field.t

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

  let schema ?default field () : (Pool_message.Error.t, t) Pool_conformist.Field.t =
    Pool_conformist.schema_decoder
      ?default
      (fun m ->
         m |> bool_of_string_opt |> CCOption.get_or ~default:false |> CCResult.return)
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
  val schema : unit -> (Pool_message.Error.t, t) Pool_conformist.Field.t
end

module String = struct
  open Sexplib.Conv

  type t = string [@@deriving eq, show, sexp_of, yojson]

  let compare = CCString.compare
  let value m = m
  let of_string m = m

  let create str =
    if CCString.is_empty str then Error Pool_message.Error.NoValue else Ok str
  ;;

  let schema field ?validation () : (Pool_message.Error.t, t) Pool_conformist.Field.t =
    let create = CCOption.value ~default:create validation in
    Pool_conformist.schema_decoder create value field
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
  val create : string -> (t, Pool_message.Error.t) result
  val value : t -> string
  val of_string : string -> t
  val schema : unit -> (Pool_message.Error.t, t) Pool_conformist.Field.t
end

module Integer = struct
  open Sexplib.Conv

  type t = int [@@deriving eq, ord, show, sexp_of, yojson]

  let value m = m
  let of_int m = m
  let to_string t = Int.to_string t

  let schema field create () : (Pool_message.Error.t, t) Pool_conformist.Field.t =
    let decode str =
      let open CCResult in
      CCInt.of_string str
      |> CCOption.to_result Pool_message.Error.(NotANumber str)
      >>= create
    in
    Pool_conformist.schema_decoder decode CCInt.to_string field
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
  val create : int -> (t, Pool_message.Error.t) result
  val value : t -> int
  val of_int : int -> t
  val compare : t -> t -> int
  val field : Pool_message.Field.t
  val schema : unit -> (Pool_message.Error.t, t) Pool_conformist.Field.t
end

module type TimeSpanSig = sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val value : t -> Ptime.Span.t
  val to_int_s : t -> int option
  val of_int_s : int -> t
  val abs : t -> t
  val of_span : Ptime.Span.t -> t
  val to_human : t -> string
  val schema : unit -> (Pool_message.Error.t, t) Pool_conformist.Field.t
end

module type TimeSig = sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val value : t -> Ptime.t
  val create : Ptime.t -> t
  val now : unit -> t
  val formatted_date_time : t -> string
  val compare : t -> t -> int
  val schema : unit -> (Pool_message.Error.t, t) Pool_conformist.Field.t
end

module type BaseSig = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> (t, Pool_message.Error.t) result
  val schema : unit -> (Pool_message.Error.t, t) Pool_conformist.Field.t
end

module type SelectorCoreTypeSig = sig
  type t

  val field : Pool_message.Field.t
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
  include Core
  include Pool_core.Selector.Make (Core)

  let create m =
    let open Pool_message in
    try Ok (read m) with
    | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, yojson) ->
      handle_ppx_yojson_err (exn, yojson)
    | _ -> Error Error.(Invalid field)
  ;;

  let schema () = Pool_conformist.schema_decoder create show field
end

(* The base type ([Time.TimeUnit]) lives in [Pool_core]; this adds the
   [Pool_message] field and the conformist schemas. *)
module TimeUnit = struct
  module Core = struct
    let field = Pool_message.Field.TimeUnit

    include Time.TimeUnit
  end

  include SelectorType (Core)
  include Core

  let of_string str =
    try Ok (read str) with
    | _ -> Error Pool_message.Error.(Invalid Core.field)
  ;;

  let named_field name = Pool_message.Field.TimeUnitOf name

  let named_schema name () =
    Pool_conformist.schema_decoder of_string show (named_field name)
  ;;

  let to_seconds value unit = Time.Duration.to_seconds (value, unit)

  let ptime_span_to_largest_unit span =
    let amount, unit = Time.Span.to_duration span in
    unit, amount
  ;;
end

module type DurationCore = sig
  val name : Pool_message.Field.t
end

module Duration (Core : DurationCore) = struct
  include Time.Span
  include Core

  let to_ptime_span value unit = Time.Span.of_duration (value, unit)

  let create m =
    if Time.Span.(equal (abs m) m) then Ok m else Error Pool_message.Error.NegativeAmount
  ;;

  let of_int_s s =
    if s >= 0
    then Ok (s |> Time.Span.of_int_s)
    else Error Pool_message.Error.NegativeAmount
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
  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val value : t -> Ptime.Span.t
  val create : Ptime.Span.t -> (t, Pool_message.Error.t) result
  val of_int_s : int -> (t, Pool_message.Error.t) result
  val of_int : int -> TimeUnit.t -> (t, Pool_message.Error.t) result
  val to_int_s : t -> int option

  val of_int_opt
    :  int option
    -> TimeUnit.t option
    -> (t option, Pool_message.Error.t) result

  val to_human : t -> string
  val to_ptime_span : int -> TimeUnit.t -> Ptime.Span.t
  val with_largest_unit : t -> TimeUnit.t * int
  val integer_schema : unit -> (Pool_message.Error.t, int) Pool_conformist.Field.t
end

module type CaqtiSig = sig
  type t

  val t : t Caqti_type.t
end
