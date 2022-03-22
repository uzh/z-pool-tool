open Tyxml.Html
module Version = Pool_common.Version
module User = Pool_user

type t =
  | Firstname of Version.t * User.Firstname.t
  | Lastname of Version.t * User.Lastname.t
  | Paused of Version.t * User.Paused.t
  | Language of
      Version.t * Pool_common.Language.t option * Pool_common.Language.t list

let hx_base_params = [ "_csrf"; "version"; "field" ]

let hx_attributes field version ?action () =
  let name = Pool_common.Message.(field_name field) in
  [ a_user_data "hx-swap" "outerHTML"
  ; a_user_data
      "hx-params"
      (CCString.concat ", " (CCList.cons name hx_base_params))
  ; a_user_data "hx-target" "closest div"
  ; a_user_data
      "hx-vals"
      (Format.asprintf
         {|{"version": "%i", "field": "%s"}|}
         (version |> Pool_common.Version.value)
         name)
  ]
  @ CCOption.(CCList.filter_map CCFun.id [ action >|= a_user_data "hx-post" ])
;;

let create m language ?(classnames = []) ?hx_post ?error () =
  let field_to_string = Pool_common.Utils.field_to_string language in
  let field_name = Pool_common.Message.field_name in
  let base_input_attributes input_type field version =
    [ a_input_type input_type
    ; a_name (field_name field)
    ; a_placeholder (field_to_string field)
    ]
    @ hx_attributes field version ?action:hx_post ()
  in
  let input, field =
    match m with
    | Firstname (version, value) ->
      let field = Pool_common.Message.Firstname in
      ( input
          ~a:
            (base_input_attributes `Text field version
            @ [ a_value (User.Firstname.value value) ])
          ()
      , field )
    | Lastname (version, value) ->
      let field = Pool_common.Message.Lastname in
      ( input
          ~a:
            (base_input_attributes `Text field version
            @ [ a_value (User.Lastname.value value) ])
          ()
      , field )
    | Paused (version, value) ->
      let field = Pool_common.Message.Paused in
      let is_checked =
        if value |> User.Paused.value then [ a_checked () ] else []
      in
      ( input ~a:(base_input_attributes `Checkbox field version @ is_checked) ()
      , field )
    | Language (version, value, tenant_languages) ->
      let field = Pool_common.Message.Language in
      ( Component.language_select
          tenant_languages
          value
          ~attributes:(hx_attributes field version ?action:hx_post ())
          ()
      , field )
  in
  let error =
    match error with
    | None -> span []
    | Some error ->
      span
        ~a:[ a_class [ "error-message" ] ]
        [ txt (error |> Pool_common.(Utils.error_to_string language)) ]
  in
  div
    ~a:[ a_class ([ "flexcolumn" ] @ classnames) ]
    [ label [ txt (field_to_string field) ]; input; error ]
;;

(* Use this CSRF element as HTMX response in POSTs*)
let csrf_element_swap csrf ?id =
  input ~a:(a_user_data "hx-swap-oob" "true" :: Component.csrf_attibs ?id csrf)
;;
