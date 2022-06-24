module Schema = struct
  module Conformist = Pool_common.Utils.PoolConformist

  let create_str str =
    if CCString.is_empty str (* TODO *)
    then Error Pool_common.Message.(Invalid Field.PublicTitle)
    else Ok str
  ;;

  let string_schema () =
    Pool_common.(
      (* TODO *)
      Utils.schema_decoder create_str CCFun.id Message.Field.PublicTitle)
  ;;

  let create_bool b =
    match b with
    | "true" -> Ok b
    | "false" -> Ok b
    | _ -> Error Pool_common.Message.(Invalid Field.PublicTitle)
  ;;

  let bool_schema () =
    Pool_common.(
      Utils.schema_decoder create_bool CCFun.id Message.Field.PublicTitle)
  ;;
end

(* TODO maybe private constructor needed *)
type single [@@deriving yojson]
type multi [@@deriving yojson]

type _ val' =
  | Str : string -> single val'
  | Nr : float -> single val'
  | Bool : bool -> single val'
  | Date : Ptime.t -> single val'
  | Empty : single val'
  (* TODO maybe problematic when pattern matching (constructor would escape its
     scope *)
  | Lst : string list -> multi val'

(* TODO: Add between operator? or make it less then and more than? *)
type _ operator =
  | Less : single operator
  | LessEqual : single operator
  | Greater : single operator
  | GreaterEqual : single operator
  | Equal : single operator
  | Like : single operator
  (* TODO maybe redundant cause have Not and EQ? *)
  | NotEqual : single operator
  | ContainsSome : multi operator
  | ContainsNone : multi operator
  | ContainsAll : multi operator

type any = Any : 'a operator -> any

let to_any t = Any t

(* TODO: use OneOf? *)
let operator_type = function
  | Any Less
  | Any LessEqual
  | Any Greater
  | Any GreaterEqual
  | Any Equal
  | Any NotEqual
  | Any Like -> `Single
  | Any ContainsSome | Any ContainsNone | Any ContainsAll -> `Multi
;;

let[@warning "-4"] to_single_operator = function
  | Any (Less as o)
  | Any (LessEqual as o)
  | Any (Greater as o)
  | Any (GreaterEqual as o)
  | Any (Equal as o)
  | Any (NotEqual as o)
  | Any (Like as o) -> o
  | _ -> failwith "Unhandled"
;;

let[@warning "-4"] to_multi_operator = function
  | Any (ContainsSome as o) | Any (ContainsNone as o) | Any (ContainsAll as o)
    -> o
  | _ -> failwith "Unhandled"
;;

let all_single_operators =
  [ Less; LessEqual; Greater; GreaterEqual; Equal; NotEqual; Like ]
;;

let all_multi_operators = [ ContainsSome; ContainsNone; ContainsAll ]

let all_operators =
  (all_single_operators |> CCList.map to_any)
  @ (all_multi_operators |> CCList.map to_any)
;;

let schema
    : type value.
      value val'
      -> unit
      -> (Pool_common.Message.error, string) Schema.Conformist.Field.t
  = function
  | Str _ -> Schema.string_schema
  | Nr _ -> Schema.string_schema
  | Bool _ -> Schema.string_schema
  | Date _ -> Schema.string_schema
  | Empty -> Schema.string_schema
  | Lst _ -> Schema.string_schema
;;

let key_to_col m fmt _ = Format.pp_print_string fmt m

type key =
  | Birthday [@name "birthday"] [@printer key_to_col "birthday"]
  | Name [@name "name"] [@printer key_to_col "name"]
  | Paused [@name "paused"] [@printer key_to_col "paused"]
  | Role [@name "role"] [@printer key_to_col "role"]
  | Verified [@name "verified_at"] [@printer key_to_col "verified_at"]
  | Tag [@name "tags"] [@printer key_to_col "tags"]
[@@deriving eq, show, enum]

let key_of_string = (* TODO[timhub]: can we reverse field show fnc? *)
  function
  | "birthday" -> Ok Birthday
  | "name" -> Ok Name
  | "paused" -> Ok Paused
  | "role" -> Ok Role
  | "verified_at" -> Ok Verified
  | "tag" -> Ok Tag
  | _ -> Error Pool_common.Message.(Invalid Field.Key)
;;

(* TODO: Error *)

let key_to_field =
  let open Pool_common.Message in
  function
  | Birthday -> Field.Birthday
  | Name -> Field.Name
  | Paused -> Field.Paused
  | Role -> Field.Role
  | Verified -> Field.VerifiedAt
  | Tag -> Field.Tag
;;

let all_keys : key list =
  CCList.range min_key max_key
  |> CCList.map key_of_enum
  |> CCList.all_some
  |> CCOption.get_exn_or "Fail"
;;

let key_to_val = function
  | Birthday -> `Date
  | Name -> `Str
  | Paused -> `Bool
  | Role -> `Str
  | Tag -> `Str
  | Verified -> `Bool
;;

let keys_with_types = all_keys |> CCList.map (fun key -> key, key |> key_to_val)

type 'a predicate = key * 'a operator * 'a val'

(* TODO turn into infix constructors *)
type filter =
  | And of filter * filter
  | Or of filter * filter
  | Not of filter
  | PredS of single predicate
  | PredM of multi predicate

let ( <&> ) a b = And (a, b)
let ( <|> ) a b = Or (a, b)

(* TODO make this prefix *)
let ( <.> ) a = Not a

(* role: only participant is shown by default
 * unverified email: only users with confirmed email are shown by default
 * paused: hidden by default
 * deactivated: hidden by default
 * tags: empty by default, depends on #23 *)

let foo : filter =
  PredS (Role, Equal, Str "participant")
  <&> Not (PredS (Verified, Equal, Empty))
;;

let bla : filter = PredS (Paused, Equal, Bool false)
let bar : filter = PredM (Tag, ContainsSome, Lst [ "pregnant" ])

module Sql = struct
  let value_to_string : type t. t val' -> string = function
    | Str str -> str
    | Nr fl -> CCFloat.to_string fl
    | Bool b -> if b then "1" else "0"
    | Date d -> Ptime.to_rfc3339 d
    | Empty -> ""
    | Lst lst -> CCString.concat ", " lst
  ;;

  let single_operator_to_sql = function
    | Less -> "<"
    | LessEqual -> "<="
    | Greater -> ">"
    | GreaterEqual -> ">="
    | Equal -> "="
    | NotEqual -> "<>"
    | Like -> "LIKE"
  ;;

  let multi_operator_to_sql = function
    | ContainsSome -> "IN"
    | ContainsNone -> "NOT IN"
    | ContainsAll -> "=" (* TODO: fix this? *)
  ;;

  let operator_to_sql : type t. t operator -> string = function
    | Less -> "<"
    | LessEqual -> "<="
    | Greater -> ">"
    | GreaterEqual -> ">="
    | Equal -> "="
    | NotEqual -> "<>"
    | Like -> "LIKE"
    | ContainsSome -> "IN"
    | ContainsNone -> "NOT IN"
    | ContainsAll -> "=" (* TODO: fix this? *)
  ;;
end

module Stringify = struct
  let operator_to_string : type t. t operator -> string = function
    | Less -> "less"
    | LessEqual -> "less or equal"
    | Greater -> "greater"
    | GreaterEqual -> "greater or equal"
    | Equal -> "equal"
    | NotEqual -> "not equal"
    | Like -> "like"
    | ContainsSome -> "contains some"
    | ContainsNone -> "contains none"
    | ContainsAll -> "contains all"
  ;;

  let operator_of_string = function
    | "less" -> Ok (Less |> to_any)
    | "less or equal" -> Ok (LessEqual |> to_any)
    | "greater" -> Ok (Greater |> to_any)
    | "greater or equal" -> Ok (GreaterEqual |> to_any)
    | "equal" -> Ok (Equal |> to_any)
    | "not equal" -> Ok (NotEqual |> to_any)
    | "like" -> Ok (Like |> to_any)
    | "contains some" -> Ok (ContainsSome |> to_any)
    | "contains none" -> Ok (ContainsNone |> to_any)
    | "contains all" -> Ok (ContainsAll |> to_any)
    | _ -> Error Pool_common.Message.(Invalid Field.Operator)
  ;;

  let any_operator_to_string = function
    | Any (Less as o)
    | Any (LessEqual as o)
    | Any (Greater as o)
    | Any (GreaterEqual as o)
    | Any (Equal as o)
    | Any (NotEqual as o)
    | Any (Like as o) -> operator_to_string o
    | Any (ContainsSome as o) | Any (ContainsNone as o) | Any (ContainsAll as o)
      -> operator_to_string o
  ;;
end

let print_single (key, operator, value) =
  Format.asprintf
    "%s %s %s"
    (show_key key)
    (Sql.operator_to_sql operator)
    (value |> Sql.value_to_string)
;;

let print_multi (key, operator, value) =
  Format.asprintf
    "%s %s (%s)"
    (show_key key)
    (Sql.operator_to_sql operator)
    (value |> Sql.value_to_string)
;;

let rec print_filter filter =
  match filter with
  | And (f1, f2) ->
    Format.asprintf "%s AND %s" (print_filter f1) (print_filter f2)
  | Or (f1, f2) ->
    Format.asprintf "%s OR %s" (print_filter f1) (print_filter f2)
  | Not f -> Format.asprintf "AND NOT %s" (print_filter f)
  | PredS pred -> print_single pred
  | PredM pred -> print_multi pred
;;

let create_filter key (operator : any) value =
  match operator_type operator with
  | `Single -> PredS (key, to_single_operator operator, Str value)
  | `Multi -> PredM (key, to_multi_operator operator, Lst [ value ])
;;

let from_urlencoded urlencoded =
  let open CCResult.Infix in
  let operator_suffix = "[operator]" in
  let value_suffix = "[value]" in
  let operators = Hashtbl.create ~random:true 5 in
  let values = Hashtbl.create ~random:true 5 in
  let get_name key str_suffix =
    CCString.take_drop CCString.(length key - length str_suffix) key
    |> fun (name, _) -> name |> key_of_string
  in
  let get_value v =
    v
    |> CCList.head_opt
    |> CCOption.to_result Pool_common.Message.(Invalid Field.Key)
    (* TODO: Error *)
  in
  let _ =
    CCList.map
      (fun (key, value) ->
        if CCString.suffix ~suf:operator_suffix key
        then (
          let name = get_name key operator_suffix in
          let value = get_value value >>= Stringify.operator_of_string in
          match CCResult.both name value with
          | Ok (name, value) -> Hashtbl.add operators name value
          | Error _ -> ())
        else if CCString.suffix ~suf:value_suffix key
        then (
          let name = get_name key value_suffix in
          let value = get_value value in
          match CCResult.both name value with
          | Ok (name, value) -> Hashtbl.add values name value
          | Error _ ->
            (* TODO[timhub]: Error handling *)
            ())
        else ())
      urlencoded
  in
  Hashtbl.fold
    (fun k v acc ->
      match Hashtbl.find_opt operators k with
      | Some operator ->
        let f = create_filter k operator v in
        CCOption.return
          (match acc with
          | Some acc -> f <&> acc
          | None -> f)
      | None -> acc)
    values
    None
  |> CCOption.map (fun f ->
         Logs.info (fun m -> m "%s" (print_filter f));
         f)
;;
