open Tyxml.Html
module Version = Pool_common.Version
module User = Pool_user

let hx_trigger = a_user_data "hx-trigger"
let hx_post = a_user_data "hx-post"
let hx_get = a_user_data "hx-get"
let hx_target = a_user_data "hx-target"
let hx_swap = a_user_data "hx-swap"
let hx_params = a_user_data "hx-params"
let hx_vals = a_user_data "hx-vals"
let hx_base_params = [ "_csrf"; "version"; "field" ]
let user_update_csrf = "_user_update_csrf"
let contact_profile_hx_post = "/user/update"

let admin_profile_hx_post id =
  Format.asprintf "/admin/contacts/%s" (id |> Pool_common.Id.value)
;;

let field_id_key = "field_id"
let custom_field_htmx_attributes id = [ field_id_key, Custom_field.Id.value id ]

let hx_attributes field version ?action ?(additional_attributes = []) () =
  let name = Pool_common.Message.Field.(field |> show) in
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

type 'a selector =
  { show : 'a -> string
  ; options : 'a list
  ; option_formatter : ('a -> string) option
  ; selected : 'a option
  }

type 'a value =
  | Text of string option
  | Number of int option
  | Checkbox of bool
  | Select of 'a selector
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
  ()
  =
  let input_class =
    match error, success with
    | None, Some _ -> [ "success" ]
    | _, _ -> []
  in
  let classnames = classnames @ input_class in
  let additional_attributes =
    [ a_class input_class ]
    @ hx_attributes
        field
        version
        ?action:hx_post
        ?additional_attributes:htmx_attributes
        ()
  in
  let default s = Option.value ~default:"" s in
  match value with
  | Text str ->
    Component.input_element
      ~classnames
      ~value:(str |> default)
      ~additional_attributes
      ?error
      ?help
      language
      `Text
      field
  | Number n ->
    Component.input_element
      ~classnames
      ~value:(n |> CCOption.map CCInt.to_string |> default)
      ~additional_attributes
      ?error
      ?help
      language
      `Number
      field
  | Checkbox boolean ->
    Component.checkbox_element
      ~additional_attributes
      ~classnames
      ~value:boolean
      ?help
      language
      field
  | Select { show; options; option_formatter; selected } ->
    Component.selector
      ~attributes:additional_attributes
      ?help
      ?option_formatter
      language
      field
      show
      options
      selected
      ()
;;

(* Use this CSRF element as HTMX response in POSTs*)
let csrf_element_swap csrf ?id =
  input ~a:(a_user_data "hx-swap-oob" "true" :: Component.csrf_attibs ?id csrf)
;;

let custom_field_to_htmx_value language =
  let open CCOption in
  let open Custom_field in
  function
  | Public.Number { Public.answer; _ } ->
    answer >|= (fun a -> a.Answer.value) |> number
  | Public.Select ({ Public.answer; _ }, options) ->
    answer
    >|= (fun a -> a.Answer.value)
    |> fun value ->
    { show = SelectOption.show_id
    ; options
    ; option_formatter = Some SelectOption.(name language)
    ; selected = value
    }
    |> select
  | Public.Text { Public.answer; _ } ->
    answer >|= (fun a -> a.Answer.value) |> text
;;

let custom_field_to_htmx ?value language custom_field =
  let to_html m = create m language in
  let open Custom_field in
  let field_id = Public.id custom_field in
  let htmx_attributes = custom_field_htmx_attributes field_id in
  let label = Public.to_common_field language custom_field in
  let version =
    Public.version custom_field
    |> CCOption.value ~default:(Pool_common.Version.create ())
  in
  let value =
    value
    |> CCOption.value
         ~default:(custom_field_to_htmx_value language custom_field)
  in
  let help = Public.to_common_hint language custom_field in
  { version
  ; field = label
  ; value
  ; htmx_attributes = Some htmx_attributes
  ; help
  }
  |> to_html
;;

let partial_update_to_htmx language sys_languages =
  let to_html m = create m language in
  let open Contact.PartialUpdate in
  let open Pool_common.Message in
  function
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
    create_entity v Field.Paused (Checkbox (paused |> User.Paused.value))
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
  | Custom field -> custom_field_to_htmx language field
;;
