module HttpUtils = Http_utils
open Tyxml.Html

let csrf_element csrf =
  input ~a:[ a_input_type `Hidden; a_name "_csrf"; a_value csrf ]
;;

let input_element input_type name value =
  let base_attributes = [ a_input_type input_type; a_value value ] in
  let attributes =
    match name with
    | Some name ->
      base_attributes
      @ [ a_name name; a_placeholder (HttpUtils.placeholder_from_name name) ]
    | None -> base_attributes
  in
  input ~a:attributes ()
;;

let hx_input_element
    input_type
    name
    value
    ?(changeset = [])
    ?hx_post
    ?hx_params
    ?hx_target
    ?(classnames = [])
    ?error
    ()
  =
  (* TODO[timhub]: select closest parent

     - https://htmx.org/attributes/hx-target/ - https://htmx.org/api/#closest *)
  let attributes =
    (match input_type with
    | `Checkbox ->
      if bool_of_string_opt value |> CCOption.get_or ~default:false
      then [ a_checked () ]
      else []
    | _ ->
      [ a_value value; a_placeholder (HttpUtils.placeholder_from_name name) ])
    @ [ a_input_type input_type
      ; a_name name
      ; a_user_data "hx-swap" "outerHTML"
      ]
    @ (if not (CCList.is_empty classnames) then [ a_class classnames ] else [])
    @ CCList.filter_map
        CCFun.id
        [ hx_params
          |> CCOption.map (fun hx_params ->
                 a_user_data
                   "hx-params"
                   (CCString.concat ", " ("changeset" :: "_csrf" :: hx_params)))
        ; hx_post |> CCOption.map (a_user_data "hx-post")
        ; hx_target
          |> CCOption.map (fun hx_target -> a_user_data "hx-target" hx_target)
        ; changeset
          |> Pool_common.ChangeSet.to_string
          |> CCString.escaped
          |> Format.asprintf {|{"changeset": "%s"}|}
          |> a_user_data "hx-vals"
          |> CCOption.some
        ]
  in
  let error =
    match error with
    | None -> span []
    | Some error ->
      span
        ~a:[ a_class [ "error-message" ] ]
        [ txt (error |> Pool_common.(Utils.error_to_string Language.En)) ]
  in
  div
    ~a:[ a_class [ "flexcolumn" ]; a_user_data "name" name ]
    [ label
        [ name
          |> HttpUtils.placeholder_from_name
          |> CCString.capitalize_ascii
          |> txt
        ]
    ; input ~a:attributes ()
    ; error
    ]
;;
