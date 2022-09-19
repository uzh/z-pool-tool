module Boolean = struct
  open Sexplib.Conv

  type t = bool [@@deriving eq, show, sexp_of, yojson]

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

  let schema field () =
    Pool_common_utils.schema_decoder
      (fun m ->
        m
        |> bool_of_string_opt
        |> CCOption.get_or ~default:false
        |> CCResult.pure)
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

  val schema
    :  unit
    -> (Entity_message.error, t) Pool_common_utils.PoolConformist.Field.t
end

module String = struct
  open Sexplib.Conv

  type t = string [@@deriving eq, show, sexp_of, yojson]

  let value m = m

  let create field str =
    if CCString.is_empty str
    then Error Entity_message.(Invalid field)
    else Ok str
  ;;

  let schema field ?validation () =
    let create = CCOption.value ~default:(create field) validation in
    Pool_common_utils.schema_decoder create value field
  ;;
end

module type StringSig = sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val create : string -> (t, Entity_message.error) result
  val value : t -> string

  val schema
    :  unit
    -> (Entity_message.error, t) Pool_common_utils.PoolConformist.Field.t
end

module type BaseSig = sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> (t, Entity_message.error) result

  val schema
    :  unit
    -> (Entity_message.error, t) Pool_common_utils.PoolConformist.Field.t
end
