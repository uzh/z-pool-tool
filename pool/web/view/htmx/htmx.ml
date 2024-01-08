open Tyxml.Html
module Version = Pool_common.Version
module User = Pool_user
module Input = Component.Input

let hx_trigger = a_user_data "hx-trigger"
let hx_post = a_user_data "hx-post"
let hx_get = a_user_data "hx-get"
let hx_target = a_user_data "hx-target"
let hx_target_closest_group = hx_target "closest .form-group"
let hx_swap = a_user_data "hx-swap"
let hx_params = a_user_data "hx-params"
let hx_vals = a_user_data "hx-vals"
let hx_base_params = [ "_csrf"; "version"; "field" ]
let contact_profile_hx_post = "/user/update"

let admin_profile_hx_post id =
  Format.asprintf "/admin/contacts/%s" (id |> Pool_common.Id.value)
;;

let admin_profile_hx_delete id field_id =
  Format.asprintf
    "%s/field/%s/delete"
    (admin_profile_hx_post id)
    Custom_field.(field_id |> Id.value)
;;

let field_id_key = "field_id"
let custom_field_htmx_attributes id = [ field_id_key, Custom_field.Id.value id ]
let multi_select_htmx_key = "multi"
let multi_select_htmx_value = "true"

let multi_select_htmx_attributes =
  [ multi_select_htmx_key, multi_select_htmx_value ]
;;

let make_hx_vals vals =
  Format.asprintf
    {|{%s}|}
    (vals
     |> CCList.map (fun (k, v) -> Format.asprintf "\"%s\": \"%s\"" k v)
     |> CCString.concat ", ")
  |> hx_vals
;;

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
  ; hx_target_closest_group
  ; make_hx_vals vals
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
  | Boolean of bool option
  | Date of Ptime.date option
  | MultiSelect of 'a Input.multi_select
  | Number of int option
  | Select of 'a selector
  | Text of string option
[@@deriving variants]

type 'a t =
  { version : Version.t
  ; field : Pool_common.Message.Field.t
  ; label : Pool_common.Message.Field.t option
  ; value : 'a value
  ; help : Pool_common.I18n.hint list option
  ; htmx_attributes : (string * string) list option
  }

let create_entity ?help ?htmx_attributes ?label version field value =
  { version; field; label; value; help; htmx_attributes }
;;

let create
  ({ version; field; label; value; help; htmx_attributes } : 'a t)
  language
  ?overridden_value
  ?promt_in_registration_hint
  ?(classnames = [])
  ?disabled
  ?error
  ?flash_values
  ?hx_post
  ?required
  ?success
  ()
  =
  let input_class =
    match error, success with
    | None, Some _ -> [ "is-valid" ]
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
  let append_html = overridden_value in
  let hints =
    help
    |> CCOption.value ~default:[]
    |> fun hints ->
    promt_in_registration_hint
    |> CCOption.map_or ~default:[] CCList.return
    |> CCList.append hints
  in
  match value with
  | Boolean boolean ->
    Input.checkbox_element
      ~additional_attributes:(additional_attributes ())
      ?append_html
      ~as_switch:true
      ~classnames
      ~hints
      ?error
      ?label_field:label
      ?required
      ?value:boolean
      ~switcher_class:input_class
      language
      field
  | Date date ->
    Input.date_picker_element
      ~additional_attributes:(additional_attributes ())
      ?append_html
      ~classnames
      ~disable_future:true
      ?error
      ~hints
      ?label_field:label
      ?required
      ?success
      ?value:date
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
      ~additional_attributes
      ?append_html
      ~classnames
      ?disabled
      ?error
      ?flash_values
      ~hints
      ?label_field:label
      ?required
      language
      t
      field
      ()
  | Number n ->
    Input.input_element
      ~classnames
      ~value:
        (CCOption.bind flash_values CCList.head_opt
         |> CCOption.value
              ~default:(n |> CCOption.map CCInt.to_string |> default))
      ~additional_attributes:(additional_attributes ())
      ?append_html
      ?error
      ?required
      ~hints
      ?label_field:label
      language
      `Number
      field
  | Select { show; options; option_formatter; selected } ->
    Input.selector
      ~add_empty:true
      ~attributes:(additional_attributes ())
      ?append_html
      ~classnames
      ?error
      ~hints
      ?label_field:label
      ?option_formatter
      ?required
      language
      field
      show
      options
      selected
      ()
  | Text str ->
    Input.input_element
      ~additional_attributes:(additional_attributes ())
      ?append_html
      ~classnames
      ?error
      ~hints
      ~value:
        (CCOption.bind flash_values CCList.head_opt
         |> CCOption.value ~default:(str |> default))
      ?required
      language
      `Text
      field
;;

let multi_select_value language options v =
  let open Custom_field in
  v
  |> CCOption.value ~default:[]
  |> fun selected ->
  Input.
    { options
    ; selected
    ; to_label = SelectOption.Public.name language
    ; to_value = SelectOption.Public.show_id
    }
  |> multiselect
;;

let select_value language options v =
  let open Custom_field in
  ({ show = SelectOption.Public.show_id
   ; options
   ; option_formatter = Some SelectOption.Public.(name language)
   ; selected = v
   }
   : 'a selector)
  |> select
;;

let field_value is_admin answer =
  let open Custom_field.Answer in
  let open CCOption in
  let { value; admin_value; _ } = answer in
  match is_admin with
  | true -> admin_value <+> value
  | false -> value
;;

let custom_field_to_htmx_value language is_admin =
  let open CCOption in
  let open Custom_field in
  function
  | Public.Boolean (_, answer) -> answer >>= field_value is_admin |> boolean
  | Public.Date (_, answer) -> answer >>= field_value is_admin |> date
  | Public.MultiSelect (_, options, answer) ->
    answer >>= field_value is_admin |> multi_select_value language options
  | Public.Number (_, answer) -> answer >>= field_value is_admin |> number
  | Public.Select (_, options, answer) ->
    answer >>= field_value is_admin |> select_value language options
  | Public.Text (_, answer) -> answer >>= field_value is_admin |> text
;;

let field_overridden_value { Custom_field.Answer.value; admin_value; _ } =
  match admin_value, value with
  | Some _, Some v -> Some (`Overridden v)
  | Some _, None -> Some `NoValue
  | _, _ -> None
;;

let custom_field_overridden_value ?hx_delete is_admin lang m =
  let delete_form () =
    let open Input in
    match is_admin, hx_delete with
    | true, Some path ->
      span
        ~a:
          [ hx_swap "outerHTML"
          ; hx_target_closest_group
          ; hx_params "_csrf"
          ; hx_post path
          ; hx_trigger "click"
          ; a_class [ "push"; "pointer"; "has-icon" ]
          ]
        [ Icon.(to_html RefreshOutline); txt "Remove admin value" ]
    | _, _ -> txt ""
  in
  match is_admin with
  | false -> None
  | true ->
    let open CCOption in
    let open Custom_field in
    let open CCFun in
    let prefix =
      Pool_common.(Utils.field_to_string lang Message.Field.OverriddenValue)
      |> CCString.capitalize_ascii
      |> txt
    in
    let add_prefix m = [ span [ strong [ prefix ]; txt ": "; m ] ] in
    let wrap html =
      div
        ~a:[ a_class [ "help"; "flexrow"; "flex-gap"; "color-orange" ] ]
        (html @ [ delete_form () ])
      |> CCList.pure
    in
    let no_value =
      Pool_common.(Utils.hint_to_string lang I18n.CustomFieldNoContactValue)
      |> txt
      |> CCList.pure
      |> wrap
    in
    let build_html to_html value =
      match value with
      | `Overridden v -> v |> to_html |> add_prefix |> wrap
      | `NoValue -> no_value
    in
    (match m with
     | Public.Boolean (_, answer) ->
       answer
       >>= field_overridden_value
       >|= build_html (Pool_common.Utils.bool_to_string lang %> txt)
     | Public.Date (_, answer) ->
       answer
       >>= field_overridden_value
       >|= build_html (Utils.Ptime.date_to_human %> txt)
     | Public.MultiSelect (_, _, answer) ->
       answer
       >>= field_overridden_value
       >|= (function
        | `NoValue -> no_value
        | `Overridden lst ->
          lst
          |> CCList.map
               (SelectOption.Public.name lang %> txt %> CCList.pure %> li)
             %> ul
             %> (fun html ->
                  [ span [ prefix; txt ":" ]
                  ; div ~a:[ a_class [ "input-group" ] ] [ html ]
                  ])
             %> wrap)
     | Public.Number (_, answer) ->
       answer >>= field_overridden_value >|= build_html (CCInt.to_string %> txt)
     | Public.Select (_, _, answer) ->
       answer
       >>= field_overridden_value
       >|= build_html (SelectOption.Public.name lang %> txt)
     | Public.Text (_, answer) ->
       answer >>= field_overridden_value >|= build_html txt)
;;

let custom_field_to_htmx
  ?version
  ?hx_post
  ?hx_delete
  ?classnames
  ?error
  ?flash_values
  ?success
  language
  is_admin
  custom_field
  =
  let required =
    Custom_field.(Public.required custom_field |> Required.value)
  in
  let overridden_value =
    custom_field_overridden_value ?hx_delete is_admin language custom_field
  in
  let promt_in_registration_hint =
    if is_admin
       && Custom_field.(
            Public.prompt_on_registration custom_field
            |> PromptOnRegistration.value)
    then Some Pool_common.I18n.CustomFieldAnsweredOnRegistration
    else None
  in
  let to_html disabled m =
    create
      ~required
      ~disabled
      ?promt_in_registration_hint
      ?overridden_value
      m
      language
  in
  let open Custom_field in
  let field_id = Public.id custom_field in
  let htmx_attributes = custom_field_htmx_attributes field_id in
  let field = Public.to_common_field language custom_field in
  let version = CCOption.value ~default:(Public.version custom_field) version in
  let disabled = Public.is_disabled is_admin custom_field in
  let value = custom_field_to_htmx_value language is_admin custom_field in
  let hints = Public.help_elements language custom_field in
  { version
  ; field
  ; label = None
  ; value
  ; htmx_attributes = Some htmx_attributes
  ; help = Some hints
  }
  |> to_html disabled ?hx_post ?classnames ?error ?flash_values ?success
;;

let partial_update_to_htmx
  language
  sys_languages
  is_admin
  partial_update
  ?hx_delete
  ?hx_post
  ?classnames
  ?error
  ?success
  =
  let open Custom_field.PartialUpdate in
  let to_html m () =
    create
      ~overridden_value:[]
      ~disabled:false
      ?classnames
      ?error
      ?hx_post
      ~required:(is_required partial_update)
      ?success
      m
      language
      ()
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
  | Language (v, lang) ->
    create_entity
      v
      ~help:[ Pool_common.I18n.ContactLanguage ]
      ~label:Field.ContactLanguage
      Field.Language
      (Select
         { show = Pool_common.Language.show
         ; options = sys_languages
         ; option_formatter = None
         ; selected = lang
         })
    |> to_html
  | Custom field ->
    custom_field_to_htmx
      ?classnames
      ?error
      ?hx_delete
      ?hx_post
      ?success
      language
      is_admin
      field
;;
