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

let print m fmt _ = Format.pp_print_string fmt m

type _ val' =
  | Str : string -> [> `Single ] val' [@printer print "str"]
  | Nr : float -> [> `Single ] val' [@printer print "nr"]
  | Bool : bool -> [> `Single ] val' [@printer print "bool"]
  | Date : Ptime.t -> [> `Single ] val' [@printer print "date"]
  | Lst : [ `Single ] val' list -> [> `Multi ] val' [@printer print "list"]
[@@deriving show { with_path = false }]

let[@warning "-4"] equal_val' val_one val_two =
  let equal_single_val' v1 v2 =
    match v1, v2 with
    | Str s1, Str s2 -> CCString.equal s1 s2
    | Nr n1, Nr n2 -> CCFloat.equal n1 n2
    | Bool b1, Bool b2 -> CCBool.equal b1 b2
    | Date d1, Date d2 ->
      CCString.equal (Ptime.to_rfc3339 d1) (Ptime.to_rfc3339 d2)
    | _ -> false
  in
  match val_one, val_two with
  | Str _, Str _ | Nr _, Nr _ | Bool _, Bool _ | Date _, Date _ ->
    equal_single_val' val_one val_two
  | Lst l1, Lst l2 -> CCList.equal equal_single_val' l1 l2
  | _ -> false
;;

let single_val_to_string (m : [ `Single ] val') =
  match m with
  | Str _ -> "str"
  | Nr _ -> "nr"
  | Bool _ -> "bool"
  | Date _ -> "date"
;;

let single_val_to_yojson (value : [ `Single ] val') : Yojson.Basic.t =
  (match value with
   | Str str -> `String str
   | Nr nr -> `Float nr
   | Bool b -> `Bool b
   | Date ptime -> `String (ptime |> Ptime.to_rfc3339))
  |> fun json -> `Assoc [ single_val_to_string value, json ]
;;

let yojson_of_val (value : [ `Single | `Multi ] val') =
  let go = single_val_to_yojson in
  match value with
  | Str str -> go (Str str)
  | Nr nr -> go (Nr nr)
  | Bool b -> go (Bool b)
  | Date ptime -> go (Date ptime)
  | Lst lst ->
    CCList.map (fun v -> single_val_to_yojson v) lst |> fun t -> `List t
;;

let single_val_of_yojson value =
  match value with
  | `Assoc [ (key, value) ] ->
    (match key, value with
     | "str", `String s -> Ok (Str s)
     | "nr", `Float f -> Ok (Nr f)
     | "nr", `Int i -> Ok (Nr (i |> CCInt.to_float))
     | "bool", `Bool b -> Ok (Bool b)
     | "date", `String s ->
       s
       |> Ptime.of_rfc3339
       |> CCResult.map (fun (d, _, _) -> Date d)
       |> CCResult.map_err (fun _ -> error)
     | _ -> Error error)
  | _ -> Error error
;;

let val_of_yojson (value : Yojson.Basic.t)
  : ([> `Single | `Multi ] val', Pool_common.Message.error) result
  =
  match value with
  | `List lst ->
    let vals =
      CCList.map single_val_of_yojson lst
      |> CCResult.flatten_l
      |> CCResult.map (fun l -> Lst l)
    in
    vals
  | _ -> single_val_of_yojson value
;;

module Key = struct
  type t =
    | Age [@printer print "age"] [@name "age"]
    | Birthday [@printer print "birthday"] [@name "birthday"]
    | Email [@printer print "email"] [@name "email"]
    | Name [@printer print "name"] [@name "name"]
    | Paused [@printer print "paused"] [@name "paused"]
    | Verified [@printer print "verified"] [@name "verified"]
    | VerifiedAt [@printer print "verified_at"] [@name "verified_at"]
  [@@deriving show { with_path = false }, eq, enum, yojson]

  let read m =
    try
      Ok
        (m
        |> Format.asprintf "[\"%s\"]"
        |> Yojson.Safe.from_string
        |> t_of_yojson)
    with
    | _ -> Error Pool_common.Message.(Invalid Field.Key)
  ;;

  let all : t list =
    CCList.range min max
    |> CCList.map of_enum
    |> CCList.all_some
    |> CCOption.get_exn_or "I18n Keys: Could not create list of all keys!"
  ;;

  let type_of_key = function
    | Age -> `Nr
    | Birthday -> `Date
    | Email | Name -> `Str
    | Paused -> `Bool
    | Verified -> `Bool
    | VerifiedAt -> `Date
  ;;
end

module Operator = struct
  type _ t =
    | Less : [> `Single ] t
    | LessEqual : [> `Single ] t
    | Greater : [> `Single ] t
    | GreaterEqual : [> `Single ] t
    | Equal : [> `Single ] t
    | NotEqual : [> `Single ] t
    | Like : [> `Single ] t
    | ContainsSome : [> `Multi ] t
    | ContainsNone : [> `Multi ] t
    | ContainsAll : [> `Multi ] t
  [@@deriving eq, enum]

  let of_string = function
    | "less" -> Ok Less
    | "less_equal" -> Ok LessEqual
    | "greater" -> Ok Greater
    | "greater_equal" -> Ok GreaterEqual
    | "equal" -> Ok Equal
    | "not_equal" -> Ok NotEqual
    | "like" -> Ok Like
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
    | Like -> "like"
    | ContainsSome -> "contains_some"
    | ContainsNone -> "contains_none"
    | ContainsAll -> "contains_all"
  ;;

  let yojson_of_t t = t |> to_string |> wrap_string |> Yojson.Safe.from_string
  let t_of_yojson m = m |> Yojson.Safe.to_string |> unwrap_string |> of_string
end

module Predicate = struct
  type 'a t = Key.t * 'a Operator.t * 'a val'

  let equal p1 p2 =
    let key1, operator1, val1 = p1 in
    let key2, operator2, val2 = p2 in
    [ Key.equal key1 key2
    ; Operator.equal operator1 operator2
    ; equal_val' val1 val2
    ]
    |> CCList.find not
  ;;

  let t_of_yojson (json : Yojson.Safe.t) =
    let json = Yojson.Safe.to_basic json |> Yojson.Basic.Util.to_list in
    match json with
    | [ key; operator; value ] ->
      let open CCResult in
      let key = key |> basic_to_safe |> Key.t_of_yojson in
      let* operator = operator |> basic_to_safe |> Operator.t_of_yojson in
      let* value = value |> val_of_yojson in
      Ok (key, operator, value)
    | _ -> Error error
  ;;

  let yojson_of_t (m : 'a t) =
    let key, operator, value = m in
    let value = value |> yojson_of_val |> basic_to_safe in
    let operator = Operator.yojson_of_t operator in
    let key = Key.yojson_of_t key in
    [ key; operator; value ]
    |> CCList.map Yojson.Safe.to_string
    |> CCString.concat ","
    |> Format.asprintf "[%s]" (* Can that be done using `List *)
    |> Yojson.Safe.from_string
  ;;
end

let print_filter m fmt _ = Format.pp_print_string fmt m

(* TODO turn into infix constructors *)
(* Should AND and OR be lists of filter? I guess UI would be easier to
   understand *)
type filter =
  | And of filter * filter [@printer print_filter "and"]
  | Or of filter * filter [@printer print_filter "or"]
  | Not of filter [@printer print_filter "not"]
  (* TODO[timhub]: Fix this type *)
  | PredS of [ `Single | `Multi ] Predicate.t [@printer print_filter "pred_s"]
  | PredM of [ `Single | `Multi ] Predicate.t [@printer print_filter "pred_m"]
[@@deriving show { with_path = false }, variants]

let[@warning "-4"] rec equal_filter f_one f_two =
  match f_one, f_two with
  | And (f_one_a, f_one_b), And (f_two_a, f_two_b)
  | Or (f_one_a, f_one_b), Or (f_two_a, f_two_b) ->
    equal_filter f_one_a f_two_a && equal_filter f_one_b f_two_b
  | Not f1, Not f2 -> equal_filter f1 f2
  | PredS p1, PredS p2 | PredM p1, PredM p2 -> Predicate.equal p1 p2
  | _ -> false
;;

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

let not_filter = Not (PredS (Key.Email, Operator.Equal, Str "test@econ.uzh.ch"))

let or_filter : filter =
  PredS (Key.Name, Operator.Equal, Str "foo") |.| not_filter
;;

let single_filter : filter = PredS (Key.Name, Operator.Equal, Str "Foo")

let list_filter : filter =
  PredM (Key.Age, Operator.ContainsNone, Lst [ Nr 20.0; Nr 21.0 ])
;;

let and_filter : filter = And (or_filter, single_filter)

(* TODO: remove this function *)
let json_to_filter json = json |> Yojson.Safe.from_string |> filter_of_yojson

type t =
  { id : Pool_common.Id.t
  ; filter : filter [@equal equal_filter]
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
