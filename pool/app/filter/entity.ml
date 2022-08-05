module Ptime = struct
  include Ptime

  let t_of_yojson m =
    m
    |> Yojson.Basic.to_string
    |> Ptime.of_rfc3339
    |> CCResult.map (fun (m, _, _) -> m)
    |> CCResult.map_err (fun _ -> "Invalid date time provided")
  ;;

  let yojson_of_t m = m |> Ptime.to_rfc3339 |> Yojson.Basic.from_string
end

let error = Pool_common.Message.(Invalid Field.Filter)
let basic_to_safe m = m |> Yojson.Basic.to_string |> Yojson.Safe.from_string
let wrap_string m = Format.asprintf "[\"%s\"]" m

(* TODO[timhub]: find better way to do this *)
let unwrap_string m =
  m |> CCString.take (CCString.length m - 2) |> CCString.drop 2
;;

type _ val' =
  | Str : string -> [> `Single ] val'
  | Nr : float -> [> `Single ] val'
  | Bool : bool -> [> `Single ] val'
  | Date : Ptime.t -> [> `Single ] val'
  | Empty : [> `Single ] val'
  (* TODO maybe problematic when pattern matching (constructor would escape its
     scope *)
  (* | Lst : 'a list -> [> `Multi ] val' *)
  (* | Lst : [> `Single ] val' list -> [> `Multi ] val' *)
  (* TODO[timhub]: Fix this type *)
  | Lst : [ `Single ] val' list -> [> `Multi ] val'

let stringify_bool = function
  | true -> "true"
  | false -> "false"
;;

let single_val_to_yojson (value : [ `Single ] val') =
  let go m = m |> wrap_string |> Yojson.Basic.from_string in
  match value with
  | Str str -> go str
  | Nr nr -> nr |> CCFloat.to_string |> go
  | Bool b -> b |> stringify_bool |> go
  | Date ptime -> ptime |> Ptime.yojson_of_t
  | Empty -> "" |> go
;;

let yojson_of_val (value : [ `Single | `Multi ] val') =
  let go = single_val_to_yojson in
  match value with
  | Str str -> go (Str str)
  | Nr nr -> go (Nr nr)
  | Bool b -> go (Bool b)
  | Date ptime -> go (Date ptime)
  | Empty -> go Empty
  | Lst lst ->
    CCList.map (fun v -> single_val_to_yojson v) lst
    |> Yojson.Basic.Util.flatten
    |> fun t -> `List t
;;

(* Why does val_of_yojson compile if I extract this part of the function? *)
(* TODO: fix date and time *)
let single_val_of_yojson value =
  match value with
  | `Bool b -> Ok (Bool b)
  | `Float f -> Ok (Nr f)
  | `Int i -> Ok (Nr (CCFloat.of_int i))
  | `Null -> Ok Empty
  | `String s -> Ok (Str s)
  | _ -> Error error
;;

(* TODO[timhub]: Use assocs and store data type? *)
let val_of_yojson (value : Yojson.Basic.t)
    : ([> `Single | `Multi ] val', Pool_common.Message.error) result
  =
  match value with
  | `Assoc _ -> Error error
  | `List lst ->
    let vals =
      CCList.map single_val_of_yojson lst
      |> CCResult.flatten_l
      |> CCResult.map (fun l -> Lst l)
    in
    vals
  | a -> single_val_of_yojson a
;;

type key = string [@@deriving yojson, eq]

let yojson_of_key m = m |> wrap_string |> Yojson.Safe.from_string
let key_of_yojson m = m |> Yojson.Safe.to_string |> unwrap_string

module Operator = struct
  type _ t =
    | Less : [> `Single ] t
    | LessEqual : [> `Single ] t
    | Greater : [> `Single ] t
    | GreaterEqual : [> `Single ] t
    | Equal : [> `Single ] t
    | NotEqual : [> `Single ] t
    | ContainsSome : [> `Multi ] t
    | ContainsNone : [> `Multi ] t
    | ContainsAll : [> `Multi ] t
  [@@deriving eq]

  let of_string = function
    | "less" -> Ok Less
    | "less_equal" -> Ok LessEqual
    | "greater" -> Ok Greater
    | "greater_equal" -> Ok GreaterEqual
    | "equal" -> Ok Equal
    | "not_equal" -> Ok NotEqual
    | "contains_some" -> Ok ContainsSome
    | "contains_none" -> Ok ContainsNone
    | "contains_all" -> Ok ContainsAll
    | _ -> Error Pool_common.Message.(Invalid Field.Operator)
  ;;

  let to_string = function
    | Less -> "less"
    | LessEqual -> "less_equal"
    | Greater -> "greater"
    | GreaterEqual -> "greater_equal"
    | Equal -> "equal"
    | NotEqual -> "not_equal"
    | ContainsSome -> "contains_some"
    | ContainsNone -> "contains_none"
    | ContainsAll -> "contains_all"
  ;;

  let yojson_of_t t = t |> to_string |> wrap_string |> Yojson.Safe.from_string
  let t_of_yojson m = m |> Yojson.Safe.to_string |> unwrap_string |> of_string
end

module Predicate = struct
  type 'a t = key * 'a Operator.t * 'a val'

  let t_of_yojson (json : Yojson.Safe.t) =
    let json = Yojson.Safe.to_basic json |> Yojson.Basic.Util.to_list in
    match json with
    | [ key; operator; value ] ->
      let open CCResult in
      let key = key |> basic_to_safe |> key_of_yojson in
      let* operator = operator |> basic_to_safe |> Operator.t_of_yojson in
      let* value = value |> val_of_yojson in
      Ok (key, operator, value)
    | _ -> Error error
  ;;

  let yojson_of_t (m : 'a t) =
    let key, operator, value = m in
    let value = value |> yojson_of_val |> basic_to_safe in
    let operator = Operator.yojson_of_t operator in
    let key = yojson_of_key key in
    [ key; operator; value ]
    |> CCList.map Yojson.Safe.to_string
    |> CCString.concat ","
    |> Format.asprintf "[%s]" (* Can that be done using `List *)
    |> Yojson.Safe.from_string
  ;;
end

let print_filter m fmt _ = Format.pp_print_string fmt m

(* TODO turn into infix constructors *)
type filter =
  | And of filter * filter [@printer print_filter "and"]
  | Or of filter * filter [@printer print_filter "or"]
  | Not of filter [@printer print_filter "not"]
  (* TODO[timhub]: Fix this type *)
  | PredS of [ `Single | `Multi ] Predicate.t [@printer print_filter "pred_s"]
  | PredM of [ `Single | `Multi ] Predicate.t [@printer print_filter "pred_m"]
[@@deriving show { with_path = false }, variants]

(* TODO[timhub]: add equality *)
let equal_filter _ _ = false

let rec yojson_of_filter (f : filter) : Yojson.Safe.t =
  (match f with
  | And (f1, f2) -> `Tuple [ f1 |> yojson_of_filter; f2 |> yojson_of_filter ]
  | Or (f1, f2) -> `Tuple [ f1 |> yojson_of_filter; f2 |> yojson_of_filter ]
  | Not f -> f |> yojson_of_filter
  | PredS p -> Predicate.yojson_of_t p
  | PredM p -> Predicate.yojson_of_t p)
  |> fun pred ->
  let k = f |> show_filter |> wrap_string |> Yojson.Safe.from_string in
  `Tuple [ k; pred ]
;;

let rec filter_of_yojson json =
  let open CCResult in
  match json with
  | `Tuple [ k; filter ] ->
    let key = k |> Yojson.Safe.to_string |> unwrap_string in
    (match key, filter with
    | "and", `Tuple [ f1; f2 ] ->
      CCResult.both (f1 |> filter_of_yojson) (f2 |> filter_of_yojson)
      >|= fun (p1, p2) -> And (p1, p2)
    | "or", `Tuple [ f1; f2 ] ->
      CCResult.both (f1 |> filter_of_yojson) (f2 |> filter_of_yojson)
      >|= fun (p1, p2) -> Or (p1, p2)
    | "not", f -> f |> filter_of_yojson >|= not
    | "pred_s", p -> p |> Predicate.t_of_yojson >|= preds
    | "pred_m", p -> p |> Predicate.t_of_yojson >|= predm
    | _ -> Error error)
  | _ -> Error error
;;

let ( &.& ) a b = And (a, b)
let ( |.| ) a b = Or (a, b)

(* TODO make this prefix *)
let ( --. ) a = Not a

(* role: only participant is shown by default
 * unverified email: only users with confirmed email are shown by default
 * paused: hidden by default
 * deactivated: hidden by default
 * tags: empty by default, depends on #23 *)

let not_filter = Not (PredS ("verified", Operator.Equal, Empty))

let or_filter : filter =
  PredS ("role", Operator.Equal, Str "participant") |.| not_filter
;;

let single_filter : filter = PredS ("paused", Operator.Equal, Bool false)

let list_filter : filter =
  PredM ("experiment_id", Operator.ContainsNone, Lst [ Nr 2.0; Nr 3.0 ])
;;

let and_filter : filter = And (or_filter, list_filter)

let json_to_filter () =
  let open CCResult in
  let json = and_filter |> yojson_of_filter in
  let* filter = filter_of_yojson json in
  Ok filter
;;

type t =
  { id : Pool_common.Id.t
  ; filter : filter
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }
[@@deriving eq, show]

let create ?(id = Pool_common.Id.create ()) filter =
  { id
  ; filter
  ; created_at = Pool_common.CreatedAt.create ()
  ; updated_at = Pool_common.UpdatedAt.create ()
  }
;;
