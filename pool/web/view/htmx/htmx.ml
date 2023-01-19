open Tyxml.Html
module Version = Pool_common.Version
module User = Pool_user
module Input = Component.Input

let hx_trigger = a_user_data "hx-trigger"
let hx_post = a_user_data "hx-post"
let hx_get = a_user_data "hx-get"
let hx_target = a_user_data "hx-target"
let hx_swap = a_user_data "hx-swap"
let hx_params = a_user_data "hx-params"
let hx_vals = a_user_data "hx-vals"
let hx_base_params = [ "_csrf"; "version"; "field" ]
let contact_profile_hx_post = "/user/update"

let admin_profile_hx_post id =
  Format.asprintf "/admin/contacts/%s" (id |> Pool_common.Id.value)
;;

let field_id_key = "field_id"
let custom_field_htmx_attributes id = [ field_id_key, Custom_field.Id.value id ]
let multi_select_key = "multi"
let multi_select_value = "true"
let multi_select_htmx_attributes = [ multi_select_key, multi_select_value ]

let base_hx_attributes name version ?action ?(additional_attributes = []) () =
  let params, vals =
    let base_vals =
      [ "version", version |> Pool_common.Version.value |> CCInt.to_string
      ; "field", name
      ]
    in
    CCList.fold_left
      (fun (params, vals) (key, value) -> key :: params, (key, value) :: vals)
      (hx_base_params, base_vals)
      additional_attributes
  in
  [ hx_swap "outerHTML"
  ; hx_params (CCString.concat ", " (CCList.cons name params))
  ; hx_target "closest .form-group"
  ; hx_vals
      (Format.asprintf
         {|{%s}|}
         (vals
         |> CCList.map (fun (k, v) -> Format.asprintf "\"%s\": \"%s\"" k v)
         |> CCString.concat ", "))
  ]
  @ CCOption.(CCList.filter_map CCFun.id [ action >|= hx_post ])
;;

let hx_attributes
  field
  version
  ?action
  ?additional_attributes
  ?(disabled = false)
  ()
  =
  if disabled
  then [ a_disabled () ]
  else (
    let name = Pool_common.Message.Field.(field |> show) in
    base_hx_attributes name version ?action ?additional_attributes ())
;;

let hx_multi_attributes field version ?action ?(additional_attributes = []) () =
  let name = Pool_common.Message.Field.(field |> array_key) in
  let additional_attributes =
    additional_attributes @ multi_select_htmx_attributes
  in
  base_hx_attributes name version ?action ~additional_attributes ()
;;

type 'a selector =
  { show : 'a -> string
  ; options : 'a list
  ; option_formatter : ('a -> string) option
  ; selected : 'a option
  }

type 'a value =
  | Boolean of bool
  | MultiSelect of 'a Input.multi_select
  | Number of int option
  | Select of 'a selector
  | Text of string option
[@@deriving variants]

type 'a t =
  { version : Version.t
  ; field : Pool_common.Message.Field.t
  ; value : 'a value
  ; help : Pool_common.I18n.hint option
  ; htmx_attributes : (string * string) list option
  }

let create_entity ?help ?htmx_attributes version field value =
  { version; field; value; help; htmx_attributes }
;;

let create
  ({ version; field; value; help; htmx_attributes } : 'a t)
  language
  ?(classnames = [])
  ?hx_post
  ?error
  ?success
  ?flash_fetcher
  ?disabled
  ?required
  ()
  =
  let input_class =
    match error, success with
    | None, Some _ -> [ "success" ]
    | _, _ -> []
  in
  let classnames = classnames @ input_class in
  let additional_attributes () =
    [ a_class input_class ]
    @ hx_attributes
        field
        version
        ?action:hx_post
        ?additional_attributes:htmx_attributes
        ?disabled
        ()
  in
  let default s = Option.value ~default:"" s in
  let fetched_value =
    CCOption.bind flash_fetcher (fun flash_fetcher ->
      field |> Pool_common.Message.Field.show |> flash_fetcher)
  in
  match value with
  | Boolean boolean ->
    Input.checkbox_element
      ~as_switch:true
      ~additional_attributes:(additional_attributes ())
      ~classnames
      ~value:boolean
      ?help
      ?error
      ?required
      language
      field
  | MultiSelect t ->
    let additional_attributes =
      [ a_class input_class ]
      @ hx_multi_attributes
          field
          version
          ?action:hx_post
          ?additional_attributes:htmx_attributes
          ()
    in
    Input.multi_select
      language
      t
      field
      ~additional_attributes
      ~classnames
      ?required
      ?error
      ?disabled
      ()
  | Number n ->
    Input.input_element
      ~classnames
      ~value:
        (fetched_value
        |> CCOption.value ~default:(n |> CCOption.map CCInt.to_string |> default)
        )
      ~additional_attributes:(additional_attributes ())
      ?error
      ?required
      ?help
      language
      `Number
      field
  | Select { show; options; option_formatter; selected } ->
    Input.selector
      ?error
      ?help
      ?option_formatter
      ?required
      ~add_empty:true
      ~attributes:(additional_attributes ())
      ~classnames
      language
      field
      show
      options
      selected
      ()
  | Text str ->
    Input.input_element
      ~classnames
      ~value:(fetched_value |> CCOption.value ~default:(str |> default))
      ~additional_attributes:(additional_attributes ())
      ?error
      ?required
      ?help
      language
      `Text
      field
;;

let custom_field_to_htmx_value language =
  let open CCOption in
  let open Custom_field in
  function
  | Public.Boolean (_, answer) ->
    answer >|= (fun a -> a.Answer.value) |> value ~default:false |> boolean
  | Public.MultiSelect (_, options, answers) ->
    answers
    |> CCList.map (fun { Answer.value; _ } -> value)
    |> fun selected ->
    Input.
      { options
      ; selected
      ; to_label = SelectOption.Public.name language
      ; to_value = SelectOption.Public.show_id
      }
    |> multiselect
  | Public.Number (_, answer) -> answer >|= (fun a -> a.Answer.value) |> number
  | Public.Select (_, options, answer) ->
    answer
    >|= (fun a -> a.Answer.value)
    |> fun value ->
    ({ show = SelectOption.Public.show_id
     ; options
     ; option_formatter = Some SelectOption.Public.(name language)
     ; selected = value
     }
      : 'a selector)
    |> select
  | Public.Text (_, answer) -> answer >|= (fun a -> a.Answer.value) |> text
;;

let custom_field_to_htmx ?version ?value language is_admin custom_field ?hx_post
  =
  let required =
    Custom_field.(Public.required custom_field |> Required.value)
  in
  let to_html disabled m = create ~required ~disabled m language in
  let open Custom_field in
  let field_id = Public.id custom_field in
  let htmx_attributes = custom_field_htmx_attributes field_id in
  let field = Public.to_common_field language custom_field in
  let version = CCOption.value ~default:(Public.version custom_field) version in
  let disabled = Public.is_disabled is_admin custom_field in
  let value =
    value
    |> CCOption.value
         ~default:(custom_field_to_htmx_value language custom_field)
  in
  let help = Public.to_common_hint language custom_field in
  { version; field; value; htmx_attributes = Some htmx_attributes; help }
  |> to_html disabled ?hx_post
;;

let partial_update_to_htmx
  language
  sys_languages
  is_admin
  partial_update
  ?hx_post
  =
  let open Contact.PartialUpdate in
  let to_html m =
    create
      ~required:(is_required partial_update)
      ?hx_post
      ~disabled:false
      m
      language
  in
  let open Pool_common.Message in
  match partial_update with
  | Firstname (v, firstname) ->
    create_entity
      v
      Field.Firstname
      (Text (firstname |> User.Firstname.value |> CCOption.pure))
    |> to_html
  | Lastname (v, lastname) ->
    create_entity
      v
      Field.Lastname
      (Text (lastname |> User.Lastname.value |> CCOption.pure))
    |> to_html
  | Paused (v, paused) ->
    create_entity v Field.Paused (Boolean (paused |> User.Paused.value))
    |> to_html
  | Language (v, lang) ->
    create_entity
      v
      Field.Language
      (Select
         { show = Pool_common.Language.show
         ; options = sys_languages
         ; option_formatter = None
         ; selected = lang
         })
    |> to_html
  | Custom field -> custom_field_to_htmx language is_admin field ?hx_post
;;
