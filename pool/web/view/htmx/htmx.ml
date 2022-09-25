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

let hx_attributes field version ?action () =
  let name = Pool_common.Message.Field.(field |> show) in
  [ hx_swap "outerHTML"
  ; hx_params (CCString.concat ", " (CCList.cons name hx_base_params))
  ; hx_target "closest .form-group"
  ; hx_vals
      (Format.asprintf
         {|{"version": "%i", "field": "%s"}|}
         (version |> Pool_common.Version.value)
         name)
  ]
  @ CCOption.(CCList.filter_map CCFun.id [ action >|= hx_post ])
;;

type 'a selector =
  { show : 'a -> string
  ; options : 'a list
  ; selected : 'a option
  }

type 'a value =
  | Text of string option
  | Number of int option
  | Checkbox of bool
  | Select of 'a selector

type 'a t = Version.t * Pool_common.Message.Field.t * 'a value

let create
  ((version, field, value) : 'a t)
  language
  ?(classnames = [])
  ?hx_post
  ?error
  ?success
  ()
  =
  let _ = Pool_common.Message.Field.CustomHtmx ("name", "Label") in
  let input_class =
    match error, success with
    | None, Some _ -> [ "success" ]
    | _, _ -> []
  in
  let classnames = classnames @ input_class in
  let additional_attributes =
    [ a_class input_class ] @ hx_attributes field version ?action:hx_post ()
  in
  let default s = Option.value ~default:"" s in
  match value with
  | Text str ->
    Component.input_element
      ~classnames
      ~value:(str |> default)
      ~additional_attributes
      ?error
      language
      `Text
      field
  | Number n ->
    Component.input_element
      ~classnames
      ~value:(n |> CCOption.map CCInt.to_string |> default)
      ~additional_attributes
      ?error
      language
      `Number
      field
  | Checkbox boolean ->
    Component.checkbox_element
      ~additional_attributes
      ~classnames
      ~value:boolean
      language
      field
  | Select { show; options; selected } ->
    Component.selector
      ~attributes:additional_attributes
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

let partial_update_to_htmx sys_languages =
  let open Contact.PartialUpdate in
  let open Pool_common.Message in
  function
  | Firstname (v, firstname) ->
    v, Field.Firstname, Text (firstname |> User.Firstname.value |> CCOption.pure)
  | Lastname (v, lastname) ->
    v, Field.Lastname, Text (lastname |> User.Lastname.value |> CCOption.pure)
  | Paused (v, paused) -> v, Field.Paused, Checkbox (paused |> User.Paused.value)
  | Language (v, lang) ->
    ( v
    , Field.Language
    , Select
        { show = Pool_common.Language.show
        ; options = sys_languages
        ; selected = lang
        } )
  | Custom _ -> failwith "TODO"
;;
