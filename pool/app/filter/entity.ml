open Ppx_yojson_conv_lib.Yojson_conv
module Id = Pool_common.Id

let model = Pool_message.Field.Filter

module Helper = struct
  let key_string = "key"
  let operator_string = "operator"
  let value_string = "value"
end

module Title = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.title
  let schema () = schema field ()
end

let read of_yojson yojson =
  yojson
  |> Yojson.Safe.to_string
  |> Format.asprintf "[%s]"
  |> Yojson.Safe.from_string
  |> of_yojson
;;

let print = Utils.ppx_printer

type single_val =
  | Bool of bool [@name "bool"] [@printer print "bool"]
  | Date of Pool_model.Base.Ptime.date [@name "date"] [@printer print "date"]
  | Language of Pool_common.Language.t [@name "language"] [@printer print "language"]
  | Nr of float [@name "nr"] [@printer print "nr"]
  | Option of Custom_field.SelectOption.Id.t [@name "option"] [@printer print "option"]
  | Str of string [@name "str"] [@printer print "str"]
[@@deriving show { with_path = false }, eq]

type value =
  | NoValue [@printer print "no_value"] [@name "no_value"]
  | Single of single_val [@printer print "single"] [@name "single"]
  | Lst of single_val list [@printer print "list"] [@name "list"]
[@@deriving show { with_path = false }, eq, variants]

let single_value_of_yojson (yojson : Yojson.Safe.t) =
  let error = Pool_message.(Error.Invalid Field.Value) in
  let open CCResult in
  match yojson with
  | `Assoc [ (key, value) ] ->
    (match key, value with
     | "bool", `Bool b -> Ok (Bool b)
     | "date", `String str ->
       str
       |> Pool_model.Base.Ptime.date_of_string
       |> CCResult.map2 (fun date -> Date date) (fun _ -> error)
     | "language", `String str ->
       str |> Pool_common.Language.create >|= fun l -> Language l
     | "nr", `Float n -> Ok (Nr n)
     | "nr", `Int n -> Ok (Nr (CCInt.to_float n))
     | "option", `String id -> Ok (Option (Custom_field.SelectOption.Id.of_string id))
     | "str", `String str -> Ok (Str str)
     | _ -> Error error)
  | _ -> Error error
;;

let value_of_yojson yojson =
  let open CCResult in
  let error = Pool_message.(Error.Invalid Field.Value) in
  match yojson with
  | `Null -> Ok NoValue
  | `Assoc _ -> single_value_of_yojson yojson >|= single
  | `List values ->
    (match values with
     | [] -> Error Pool_message.Error.FilterListValueMustNotBeEmpty
     | values -> values |> CCList.map single_value_of_yojson |> CCList.all_ok >|= lst)
  | _ -> Error error
;;

let to_assoc key value = `Assoc [ key, value ]

let yojson_of_single_val = function
  | Bool b -> `Bool b
  | Date date -> `String (Pool_model.Base.Ptime.date_to_string date)
  | Language lang -> `String (Pool_common.Language.show lang)
  | Nr n -> `Float n
  | Option id -> `String (Custom_field.SelectOption.Id.value id)
  | Str str -> `String str
;;

let yojson_of_value m =
  let to_json value = to_assoc (value |> show_single_val) (yojson_of_single_val value) in
  match m with
  | NoValue -> `Null
  | Single single -> to_json single
  | Lst values -> `List (CCList.map to_json values)
;;

module Key = struct
  type input_type =
    | Bool [@printer print "bool"]
    | Date [@printer print "date"]
    | Languages of Pool_common.Language.t list [@printer print "language"]
    | Nr [@printer print "nr"]
    | Str [@printer print "str"]
    | Select of Custom_field.SelectOption.t list [@printer print "option"]
    | MultiSelect of Custom_field.SelectOption.t list [@printer print "multi_select"]
    | QueryExperiments
    | QueryTags
  [@@deriving show]

  type hardcoded =
    | ContactLanguage [@printer print "contact_language"] [@name "contact_language"]
    | Firstname [@printer print "first_name"] [@name "first_name"]
    | Name [@printer print "name"] [@name "name"]
    | NumAssignments [@printer print "num_assignments"] [@name "num_assignments"]
    | NumInvitations [@printer print "num_invitations"] [@name "num_invitations"]
    | NumNoShows [@printer print "num_no_shows"] [@name "num_no_shows"]
    | NumParticipations [@printer print "num_participations"] [@name "num_participations"]
    | NumShowUps [@printer print "num_show_ups"] [@name "num_show_ups"]
    | Participation [@printer print "participation"] [@name "participation"]
    | Assignment [@printer print "assignment"] [@name "assignment"]
    | Invitation [@printer print "invitation"] [@name "invitation"]
    | Tag [@printer print "tag"] [@name "tag"]
  [@@deriving show { with_path = false }, eq, yojson, variants, enum]

  type human =
    | CustomField of Custom_field.t
    | Hardcoded of hardcoded
  [@@deriving show { with_path = false }, eq, variants]

  type t =
    | CustomField of Custom_field.Id.t
    | Hardcoded of hardcoded
  [@@deriving show { with_path = false }, eq]

  let read_hardcoded yojson =
    try Some (yojson |> read hardcoded_of_yojson) with
    | _ -> None
  ;;

  let to_human key_list m =
    let find_in_keys key_id =
      CCList.find_opt
        (fun key ->
           match (key : human) with
           | Hardcoded _ -> false
           | CustomField f -> Custom_field.(Id.equal (id f) key_id))
        key_list
    in
    match (m : t) with
    | Hardcoded h -> Some (Hardcoded h : human)
    | CustomField id -> id |> find_in_keys
  ;;

  let of_yojson : Yojson.Safe.t -> (t, Pool_message.Error.t) result =
    fun yojson ->
    match read_hardcoded yojson with
    | Some h -> Ok (Hardcoded h)
    | None ->
      (* The "validate_query" function will check, if the id belongs to an existing custom
         field *)
      (match yojson with
       | `String id -> Ok (CustomField (id |> Custom_field.Id.of_string))
       | _ -> Error Pool_message.(Error.Invalid Field.Key))
  ;;

  let to_yojson (m : t) =
    (match m with
     | Hardcoded h -> h |> show_hardcoded
     | CustomField id -> id |> Custom_field.Id.value)
    |> fun str -> `String str
  ;;

  let read m =
    try Some (`String m |> read hardcoded_of_yojson) with
    | _ -> None
  ;;

  let human_to_label language human =
    let open CCString in
    match (human : human) with
    | Hardcoded h -> show_hardcoded h |> replace ~sub:"_" ~by:" " |> capitalize_ascii
    | CustomField f -> Custom_field.(f |> name_value language)
  ;;

  let human_to_value human =
    match (human : human) with
    | Hardcoded h -> show_hardcoded h
    | CustomField f -> Custom_field.(f |> id |> Id.value)
  ;;

  let hardcoded_to_single_value_sql = function
    | ContactLanguage -> Ok "pool_contacts.language"
    | Firstname -> Ok "user_users.given_name"
    | Name -> Ok "user_users.name"
    | NumAssignments -> Ok "pool_contacts.num_assignments"
    | NumInvitations -> Ok "pool_contacts.num_invitations"
    | NumNoShows -> Ok "pool_contacts.num_no_shows"
    | NumParticipations -> Ok "pool_contacts.num_participations"
    | NumShowUps -> Ok "pool_contacts.num_show_ups"
    | Assignment | Invitation | Participation | Tag ->
      Error Pool_message.(Error.QueryNotCompatible (Field.Key, Field.Value))
  ;;

  let type_of_hardcoded m : input_type =
    match m with
    | ContactLanguage -> Languages Pool_common.Language.all
    | Firstname -> Str
    | Name -> Str
    | NumAssignments | NumInvitations | NumNoShows | NumParticipations | NumShowUps -> Nr
    | Assignment | Invitation | Participation -> QueryExperiments
    | Tag -> QueryTags
  ;;

  let type_of_custom_field m : input_type =
    let open Custom_field in
    match m with
    | Boolean _ -> Bool
    | Date _ -> Date
    | Number _ -> Nr
    | MultiSelect (_, options) -> MultiSelect options
    | Select (_, options) -> Select options
    | Text _ -> Str
  ;;

  let type_of_key m : input_type =
    match (m : human) with
    | CustomField c -> type_of_custom_field c
    | Hardcoded h -> type_of_hardcoded h
  ;;

  let validate_value (key_list : human list) (key : t) value =
    let error = Pool_message.(Error.QueryNotCompatible (Field.Value, Field.Key)) in
    let open CCResult in
    let validate_single_value input_type value =
      match[@warning "-4"] value, input_type with
      | (Bool _ : single_val), (Bool : input_type) | Date _, Date | Nr _, Nr | Str _, Str
        -> Ok ()
      | Language lang, Languages languages ->
        CCList.find_opt (Pool_common.Language.equal lang) languages
        |> CCOption.to_result error
        >|= CCFun.const ()
      | Option selected, Select options | Option selected, MultiSelect options ->
        CCList.find_opt
          (fun option -> Custom_field.SelectOption.(Id.equal option.id selected))
          options
        |> CCOption.to_result error
        >|= CCFun.const ()
      | Str _, (QueryExperiments | QueryTags) -> Ok ()
      | _ -> Error error
    in
    let validate value input_type =
      match value with
      | NoValue -> Ok ()
      | Single v -> validate_single_value input_type v
      | Lst lst ->
        lst
        |> CCList.map (validate_single_value input_type)
        |> CCList.all_ok
        >|= CCFun.const ()
    in
    match key with
    | Hardcoded v -> v |> type_of_hardcoded |> validate value
    | CustomField field_id ->
      let* custom_field =
        CCList.find_map
          (fun (key : human) ->
             match key with
             | Hardcoded _ -> None
             | CustomField field ->
               if Custom_field.(Id.equal (id field) field_id) then Some field else None)
          key_list
        |> CCOption.to_result Pool_message.(Error.Invalid Field.Key)
      in
      custom_field |> type_of_custom_field |> validate value
  ;;

  let all_hardcoded : hardcoded list =
    CCList.range min_hardcoded max_hardcoded
    |> CCList.map hardcoded_of_enum
    |> CCList.all_some
    |> CCOption.get_exn_or "Hardcoded filter keys: Could not create list of all keys!"
  ;;
end

module Operator = struct
  open CCList.Infix

  let generate_all min max of_enum =
    CCList.range min max
    |> CCList.map of_enum
    |> CCList.all_some
    |> CCOption.get_exn_or "Could not create list of all operators!"
  ;;

  module Equality = struct
    type t =
      | Equal [@printer print "equal"] [@name "equal"]
      | NotEqual [@printer print "not_equal"] [@name "not_equal"]
    [@@deriving show { with_path = false }, eq, enum, yojson]

    let all = generate_all min max of_enum
    let json_key = "Equality"

    let read yojson =
      try Some (yojson |> read t_of_yojson) with
      | _ -> None
    ;;

    let to_sql = function
      | Equal -> "="
      | NotEqual -> "<>"
    ;;

    let to_human = function
      | Equal -> "equal"
      | NotEqual -> "not equal"
    ;;
  end

  module Existence = struct
    type t =
      | Empty [@printer print "empty"] [@name "empty"]
      | NotEmpty [@printer print "not_empty"] [@name "not_empty"]
    [@@deriving show { with_path = false }, eq, enum, yojson]

    let all = generate_all min max of_enum
    let json_key = "Existence"

    let read yojson =
      try Some (yojson |> read t_of_yojson) with
      | _ -> None
    ;;

    let to_sql = function
      | Empty -> "IS NULL"
      | NotEmpty -> "IS NOT NULL"
    ;;

    let to_human = function
      | Empty -> "empty"
      | NotEmpty -> "not empty"
    ;;
  end

  module ListM = struct
    type t =
      | ContainsSome [@printer print "contains_some"] [@name "contains_some"]
      | ContainsNone [@printer print "contains_none"] [@name "contains_none"]
      | ContainsAll [@printer print "contains_all"] [@name "contains_all"]
    [@@deriving show { with_path = false }, eq, enum, yojson]

    let all = generate_all min max of_enum
    let single_select_operators = [ ContainsSome; ContainsNone ]
    let json_key = "List"

    let read yojson =
      try Some (yojson |> read t_of_yojson) with
      | _ -> None
    ;;

    let to_sql = function
      (* List operators are used to query custom field answers by their value which store
         json arrays *)
      | ContainsSome | ContainsAll -> "LIKE"
      | ContainsNone -> "NOT LIKE"
    ;;

    let to_human = function
      | ContainsSome -> "contains some"
      | ContainsNone -> "contains none"
      | ContainsAll -> "contains all"
    ;;
  end

  module Size = struct
    type t =
      | Less [@printer print "less"] [@name "less"]
      | LessEqual [@printer print "less_equal"] [@name "less_equal"]
      | Greater [@printer print "greater"] [@name "greater"]
      | GreaterEqual [@printer print "greater_equal"] [@name "greater_equal"]
    [@@deriving show { with_path = false }, eq, enum, yojson]

    let all = generate_all min max of_enum
    let json_key = "Size"

    let read yojson =
      try Some (yojson |> read t_of_yojson) with
      | _ -> None
    ;;

    let to_sql = function
      | Less -> "<"
      | LessEqual -> "<="
      | Greater -> ">"
      | GreaterEqual -> ">="
    ;;

    let to_human = function
      | Less -> "less"
      | LessEqual -> "less or equal"
      | Greater -> "greater"
      | GreaterEqual -> "greater or equal"
    ;;
  end

  module StringM = struct
    type t = Like [@printer print "like"] [@name "like"]
    [@@deriving show { with_path = false }, eq, enum, yojson]

    let all = generate_all min max of_enum
    let json_key = "String"

    let read yojson =
      try Some (yojson |> read t_of_yojson) with
      | _ -> None
    ;;

    let to_sql = function
      | Like -> "LIKE"
    ;;

    let to_human = function
      | Like -> "like"
    ;;
  end

  type t =
    | Equality of Equality.t
    | Existence of Existence.t
    | List of ListM.t
    | Size of Size.t
    | String of StringM.t
  [@@deriving show { with_path = false }, eq, yojson, variants]

  let show = function
    | Equality o -> Equality.show o
    | String o -> StringM.show o
    | Size o -> Size.show o
    | List o -> ListM.show o
    | Existence o -> Existence.show o
  ;;

  let to_sql = function
    | Equality o -> Equality.to_sql o
    | String o -> StringM.to_sql o
    | Size o -> Size.to_sql o
    | List o -> ListM.to_sql o
    | Existence o -> Existence.to_sql o
  ;;

  let to_human m =
    (match m with
     | Equality o -> Equality.to_human o
     | String o -> StringM.to_human o
     | Size o -> Size.to_human o
     | List o -> ListM.to_human o
     | Existence o -> Existence.to_human o)
    |> CCString.capitalize_ascii
  ;;

  let all_equality_operators = Equality.all >|= equality
  let all_string_operators = StringM.all >|= string
  let all_size_operators = Size.all >|= size
  let all_list_operators = ListM.all >|= list
  let all_select_operators = ListM.single_select_operators >|= list
  let all_existence_operators = Existence.all >|= existence

  let all =
    let open CCList in
    all_equality_operators
    @ all_string_operators
    @ all_size_operators
    @ all_list_operators
    @ all_existence_operators
  ;;

  let encode_operators =
    let open CCOption.Infix in
    [ Equality.(all |> CCList.map show, fun y -> y |> read >|= equality)
    ; StringM.(all |> CCList.map show, fun y -> y |> read >|= string)
    ; Size.(all |> CCList.map show, fun y -> y |> read >|= size)
    ; ListM.(all |> CCList.map show, fun y -> y |> read >|= list)
    ; Existence.(all |> CCList.map show, fun y -> y |> read >|= existence)
    ]
    |> CCList.flat_map (fun (operators, fnc) ->
      CCList.map (fun operator -> operator, fnc) operators)
  ;;

  let of_yojson yojson =
    let rec find_operator item = function
      | [] -> None
      | (operator, fnc) :: _ when CCString.equal operator item -> fnc yojson
      | _ :: tl -> find_operator item tl
    in
    (match yojson with
     | `String str -> find_operator str encode_operators
     | _ -> None)
    |> CCOption.to_result Pool_message.(Error.Invalid Field.Operator)
  ;;

  let yojson_of_t : t -> Yojson.Safe.t = CCFun.(show %> fun str -> `String str)

  let operator_of_hardcoded =
    let open Key in
    function
    | ContactLanguage -> all_equality_operators @ all_existence_operators
    | Firstname | Name -> all_equality_operators @ all_string_operators
    | NumAssignments | NumInvitations | NumNoShows | NumParticipations | NumShowUps ->
      all_equality_operators @ all_size_operators
    | Participation | Tag | Invitation | Assignment -> all_list_operators
  ;;

  let input_type_to_operator (key : Key.input_type) =
    let open Key in
    match key with
    | Bool | Languages _ -> all_equality_operators
    | Date | Nr -> all_equality_operators @ all_size_operators
    | MultiSelect _ | QueryExperiments | QueryTags -> all_list_operators
    | Select _ -> all_select_operators
    | Str -> all_equality_operators @ all_string_operators
  ;;

  let operators_of_key : Key.human -> t list =
    let open Key in
    function
    | CustomField field ->
      (field |> type_of_custom_field |> input_type_to_operator) @ all_existence_operators
    | Hardcoded hardcoded -> hardcoded |> type_of_hardcoded |> input_type_to_operator
  ;;

  let validate (key : Key.t) operator =
    let msg = Pool_message.(Error.QueryNotCompatible Field.(Operator, Key)) in
    match key with
    | Key.Hardcoded key ->
      key
      |> operator_of_hardcoded
      |> CCList.mem ~eq:equal operator
      |> Utils.Bool.to_result msg
    | Key.CustomField _ -> Ok ()
  ;;

  let value_disabled = function
    | Existence _ -> true
    | Equality _ | String _ | Size _ | List _ -> false
  ;;
end

module Predicate = struct
  type t =
    { key : Key.t
    ; operator : Operator.t
    ; value : value
    }
  [@@deriving eq]

  type human =
    { key : Key.human option
    ; operator : Operator.t option
    ; value : value option
    }
  [@@deriving eq]

  let create key operator value : t = { key; operator; value }
  let create_human ?key ?operator ?value () : human = { key; operator; value }

  let validate : t -> Key.human list -> (t, Pool_message.Error.t) result =
    fun ({ key; operator; value } as m) key_list ->
    let open CCResult in
    let* () = Key.validate_value key_list key value in
    let* () = Operator.validate key operator in
    Ok m
  ;;

  let t_of_yojson (yojson : Yojson.Safe.t) =
    let open Helper in
    let to_result field =
      CCOption.value ~default:(Error (Pool_message.Error.Invalid field))
    in
    match yojson with
    | `Assoc assoc ->
      let open CCResult in
      let go json_key of_yojson =
        assoc |> CCList.assoc_opt ~eq:CCString.equal json_key |> CCOption.map of_yojson
      in
      let* key = go key_string Key.of_yojson |> to_result Pool_message.Field.Key in
      let* operator =
        go operator_string Operator.of_yojson |> to_result Pool_message.Field.Operator
      in
      let* value =
        go value_string value_of_yojson
        |> fun opt ->
        let open Operator in
        let error = Error Pool_message.(Error.Invalid Field.Value) in
        match operator with
        | Existence _ -> opt |> CCOption.value ~default:(Ok NoValue)
        | Equality _ | String _ | Size _ | List _ -> CCOption.value ~default:error opt
      in
      Ok (create key operator value)
    | _ -> Error Pool_message.(Error.Invalid Field.Predicate)
  ;;

  let yojson_of_t ({ key; operator; value } : t) =
    let key = Key.to_yojson key in
    let operator = Operator.yojson_of_t operator in
    let value = yojson_of_value value in
    `Assoc Helper.[ key_string, key; operator_string, operator; value_string, value ]
  ;;
end

type query =
  | And of query list [@printer print "and"] [@name "and"]
  | Or of query list [@printer print "or"] [@name "or"]
  | Not of query [@printer print "not"] [@name "not"]
  | Pred of Predicate.t [@printer print "pred"] [@name "pred"]
  | Template of Pool_common.Id.t [@printer print "template"] [@name "template"]
[@@deriving show { with_path = false }, variants, eq]

let rec yojson_of_query f : Yojson.Safe.t =
  (match f with
   | And queries -> `List (CCList.map yojson_of_query queries)
   | Or queries -> `List (CCList.map yojson_of_query queries)
   | Not f -> f |> yojson_of_query
   | Pred p -> Predicate.yojson_of_t p
   | Template id -> `String (Pool_common.Id.value id))
  |> fun pred -> `Assoc [ f |> show_query, pred ]
;;

let rec query_of_yojson json =
  let error = Pool_message.(Error.Invalid Field.Query) in
  let open CCResult.Infix in
  let not_empty l =
    match l with
    | [] -> Error Pool_message.Error.FilterAndOrMustNotBeEmpty
    | _ -> Ok l
  in
  let to_query_list json =
    json |> not_empty >>= CCFun.(CCList.map query_of_yojson %> CCList.all_ok)
  in
  match json with
  | `Assoc [ (key, filter) ] ->
    (match key, filter with
     | "and", `List json -> json |> to_query_list >|= fun lst -> And lst
     | "or", `List json -> json |> to_query_list >|= fun lst -> Or lst
     | "not", f -> f |> query_of_yojson >|= not
     | "pred", p -> p |> Predicate.t_of_yojson >|= pred
     | "template", `String id ->
       id |> Pool_common.Id.of_string |> template |> CCResult.return
     | _ -> Error error)
  | _ -> Error error
;;

let query_of_string str = str |> Yojson.Safe.from_string |> query_of_yojson

type t =
  { id : Pool_common.Id.t
  ; query : query
  ; title : Title.t option
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show, yojson_of]

let create ?(id = Pool_common.Id.create ()) title query =
  { id
  ; query
  ; title
  ; created_at = Pool_common.CreatedAt.create_now ()
  ; updated_at = Pool_common.UpdatedAt.create_now ()
  }
;;

let rec validate_query key_list (template_list : t list) =
  let open CCResult in
  let validate_list fnc queries =
    queries |> CCList.map (validate_query key_list template_list) |> CCList.all_ok >|= fnc
  in
  function
  | And queries -> validate_list (fun lst -> And lst) queries
  | Or queries -> validate_list (fun lst -> Or lst) queries
  | Not f -> validate_query key_list template_list f >|= not
  | Pred p -> Predicate.validate p key_list >|= pred
  | Template filter_id ->
    CCList.find_opt (fun f -> Pool_common.Id.equal f.id filter_id) template_list
    |> CCOption.to_result Pool_message.(Error.NotFound Field.Filter)
    >|= CCFun.const (template filter_id)
;;

let rec contains_template = function
  | And queries | Or queries ->
    CCList.map contains_template queries
    |> CCList.filter CCFun.id
    |> fun lst -> CCList.length lst > 0
  | Not p -> contains_template p
  | Pred _ -> false
  | Template _ -> true
;;

type base_condition =
  | MatchesFilter
  | Matcher of Pool_common.Id.t
  | MatcherReset of Pool_common.Id.t * Ptime.t
[@@deriving eq, show]

open Pool_message

let column_title = (Field.Title, "pool_filter.title") |> Query.Column.create
let column_created_at = (Field.CreatedAt, "pool_filter.created_at") |> Query.Column.create
let filterable_by = None
let searchable_by = [ column_title ]
let sortable_by = column_created_at :: searchable_by
let default_sort = Query.Sort.{ column = column_created_at; order = SortOrder.Descending }
let default_query = Query.create ~sort:default_sort ()
