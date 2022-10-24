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
let multi_select_key = "multi"
let multi_select_value = "true"
let multi_select_htmx_attributes = [ multi_select_key, multi_select_value ]

let hx_attributes
  field
  version
  ?action
  ?(additional_attributes = [])
  ?(disabled = false)
  ()
  =
  if disabled
  then [ a_disabled () ]
  else (
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
    @ CCOption.(CCList.filter_map CCFun.id [ action >|= hx_post ]))
;;

(* TODO :Can I use to common field here as well? *)
type 'a selector =
  { show : 'a -> string
  ; options : 'a list
  ; option_formatter : ('a -> string) option
  ; selected : 'a option
  }

type 'a multi_select =
  { show : 'a -> string
  ; options : 'a list
  ; selected : 'a list
  ; to_common_field : 'a -> Pool_common.Message.Field.t
  }

type 'a value =
  | Boolean of bool
  | MultiSelect of 'a multi_select
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

let multi_select
  language
  { show; options; selected; to_common_field }
  field
  version
  htmx_attributes
  ?(classnames = [])
  ?hx_post
  ?error
  ?disabled
  ()
  =
  let error = Component.Elements.error language error in
  CCList.map
    (fun option ->
      let field = to_common_field option in
      let name = field |> Pool_common.Message.Field.show in
      let is_checked =
        CCList.mem
          ~eq:(fun o1 o2 -> CCString.equal (show o1) (show o2))
          option
          selected
      in
      let htmx_attributes =
        hx_attributes
          field
          version
          ?additional_attributes:htmx_attributes
          ?action:hx_post
          ?disabled
          ()
      in
      let input_elm =
        let checked = if is_checked then [ a_checked () ] else [] in
        input
          ~a:
            ([ a_input_type `Checkbox; a_name name; a_id name ]
            @ checked
            @ htmx_attributes)
          ()
      in
      let label =
        label
          ~a:[ a_label_for name ]
          [ txt Pool_common.(Utils.field_to_string language field) ]
      in
      div [ input_elm; label ])
    options
  |> fun options_html ->
  div
    ~a:[ a_class ([ "form-group"; "horizontal" ] @ classnames) ]
    [ label [ txt Pool_common.(Utils.field_to_string language field) ]
    ; div ~a:[ a_class [ "input-group" ] ] (options_html @ error)
    ]
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
    Component.checkbox_element
      ~as_switch:true
      ~orientation:`Horizontal
      ~additional_attributes
      ~classnames
      ~value:boolean
      ?help
      ?error
      language
      field
  | MultiSelect t ->
    multi_select
      language
      t
      field
      version
      (htmx_attributes
      |> CCOption.map_or
           ~default:multi_select_htmx_attributes
           (CCList.append multi_select_htmx_attributes)
      |> CCOption.pure)
      ~classnames
      ?hx_post
      ?error
      ?disabled
      ()
  | Number n ->
    Component.input_element
      ~classnames
      ~value:
        (fetched_value
        |> CCOption.value ~default:(n |> CCOption.map CCInt.to_string |> default)
        )
      ~additional_attributes
      ?error
      ?help
      language
      `Number
      field
  | Select { show; options; option_formatter; selected } ->
    Component.selector
      ~attributes:additional_attributes
      ?help
      ?option_formatter
      ~add_empty:true
      language
      field
      show
      options
      selected
      ()
  | Text str ->
    Component.input_element
      ~classnames
      ~value:(fetched_value |> CCOption.value ~default:(str |> default))
      ~additional_attributes
      ?error
      ?help
      language
      `Text
      field
;;

(* Use this CSRF element as HTMX response in POSTs*)
let csrf_element_swap csrf ?id =
  input ~a:(a_user_data "hx-swap-oob" "true" :: Component.csrf_attibs ?id csrf)
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
    ({ show = SelectOption.show_id
     ; options
     ; selected
     ; to_common_field = SelectOption.to_common_field language
     }
      : 'a multi_select)
    |> multiselect
  | Public.Number (_, answer) -> answer >|= (fun a -> a.Answer.value) |> number
  | Public.Select (_, options, answer) ->
    answer
    >|= (fun a -> a.Answer.value)
    |> fun value ->
    ({ show = SelectOption.show_id
     ; options
     ; option_formatter = Some SelectOption.(name language)
     ; selected = value
     }
      : 'a selector)
    |> select
  | Public.Text (_, answer) -> answer >|= (fun a -> a.Answer.value) |> text
;;

let custom_field_to_htmx ?version ?value language is_admin custom_field =
  let to_html disabled m = create ~disabled m language in
  let open Custom_field in
  let field_id = Public.id custom_field in
  let htmx_attributes = custom_field_htmx_attributes field_id in
  let label = Public.to_common_field language custom_field in
  let version = CCOption.value ~default:(Public.version custom_field) version in
  let value =
    value
    |> CCOption.value
         ~default:(custom_field_to_htmx_value language custom_field)
  in
  let disabled = Public.is_disabled is_admin custom_field in
  let help = Public.to_common_hint language custom_field in
  { version
  ; field = label
  ; value
  ; htmx_attributes = Some htmx_attributes
  ; help
  }
  |> to_html disabled
;;

let partial_update_to_htmx language sys_languages is_admin =
  let to_html (m : 'a t) = create ~disabled:false m language in
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
  | Custom field -> custom_field_to_htmx language is_admin field
;;
