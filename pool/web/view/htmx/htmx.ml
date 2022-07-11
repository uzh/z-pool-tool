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

type t =
  | Firstname of Version.t * User.Firstname.t
  | Lastname of Version.t * User.Lastname.t
  | Paused of Version.t * User.Paused.t
  | Language of
      Version.t * Pool_common.Language.t option * Pool_common.Language.t list

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

(* TODO[timhub]:

   - Create multiple htmx input types? - Text - Checkbox - Select? *)
let create
    field
    language
    version
    ?value
    ?checked
    ?(classnames = [])
    ?hx_post
    ?error
    ?success
    ()
  =
  let open Pool_common in
  let field_to_string = Utils.field_to_string language in
  let value = CCOption.value ~default:"" value in
  let checked = CCOption.value ~default:false checked in
  let input_class =
    match error, success with
    | Some _, None -> [ "has-error" ]
    | None, Some _ -> [ "success" ]
    | _, _ -> []
  in
  let error_msg =
    match error with
    | None -> span []
    | Some error ->
      span
        ~a:[ a_class [ "help"; "error-message" ] ]
        [ txt (error |> Pool_common.(Utils.error_to_string language)) ]
  in
  let base_input_attributes input_type field version =
    [ a_input_type input_type
    ; a_name Message.Field.(field |> show)
    ; a_placeholder (field_to_string field)
    ; a_class input_class
    ]
    @ hx_attributes field version ?action:hx_post ()
  in
  let input, field =
    let open Pool_common.Message in
    match[@warning "-4"] field with
    | Field.Firstname ->
      let field = Message.Field.Firstname in
      ( input
          ~a:(base_input_attributes `Text field version @ [ a_value value ])
          ()
      , field )
    | Field.Lastname ->
      let field = Message.Field.Lastname in
      ( input
          ~a:(base_input_attributes `Text field version @ [ a_value value ])
          ()
      , field )
    | Field.Paused ->
      let field = Message.Field.Paused in
      let is_checked = if checked then [ a_checked () ] else [] in
      ( input ~a:(base_input_attributes `Checkbox field version @ is_checked) ()
      , field )
    | Field.Language ->
      let field = Message.Field.Language in
      ( Component.language_select
          Pool_common.Language.all (* TODO[timhub]: Use correct languages *)
          (value |> Pool_common.Language.create |> CCResult.to_opt)
          ~field
          ~attributes:(hx_attributes field version ?action:hx_post ())
          ()
      , field )
    | Field.Custom _ -> failwith "Todo"
    | _ -> failwith "Todo"
  in
  div
    ~a:[ a_class ([ "form-group" ] @ classnames) ]
    [ label [ txt (field_to_string field |> CCString.capitalize_ascii) ]
    ; input
    ; error_msg
    ]
;;

(* Use this CSRF element as HTMX response in POSTs*)
let csrf_element_swap csrf ?id =
  input ~a:(a_user_data "hx-swap-oob" "true" :: Component.csrf_attibs ?id csrf)
;;
