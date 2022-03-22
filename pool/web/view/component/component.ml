module HttpUtils = Http_utils
open Tyxml.Html

let htmx_attributes name version ?action () =
  let hx_params = [ "_csrf"; "version"; "field" ] in
  [ a_user_data "hx-swap" "outerHTML"
  ; a_user_data "hx-params" (CCString.concat ", " (CCList.cons name hx_params))
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

let language_select
    language
    name
    options
    input_label
    ~selected
    ?(attributes = [])
    ?(classnames = [])
    ?error
    ()
  =
  let select =
    select
      ~a:
        ([ a_name name ]
        @ attributes
        @
        if not (CCList.is_empty classnames) then [ a_class classnames ] else []
        )
      (CCList.map
         (fun l ->
           let is_selected =
             selected
             |> CCOption.map (fun selected ->
                    if Pool_common.Language.equal selected l
                    then [ a_selected () ]
                    else [])
             |> CCOption.value ~default:[]
           in
           option
             ~a:([ a_value (Pool_common.Language.code l) ] @ is_selected)
             (txt (Pool_common.Language.code l)))
         options)
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
    ~a:[ a_class [ "flexcolumn" ] ]
    [ label [ txt Pool_common.(Utils.field_to_string language input_label) ]
    ; select
    ; error
    ]
;;

let csrf_attibs ?id csrf =
  let attribs = [ a_input_type `Hidden; a_name "_csrf"; a_value csrf ] in
  match id with
  | Some id -> a_id id :: attribs
  | None -> attribs
;;

let csrf_element csrf ?id = input ~a:(csrf_attibs ?id csrf)

(* Use this CSRF element as HTMX response in POSTs*)
let csrf_element_swap csrf ?id =
  input ~a:(a_user_data "hx-swap-oob" "true" :: csrf_attibs ?id csrf)
;;

let input_element language input_type name input_label value =
  let input_label = Pool_common.Utils.field_to_string language input_label in
  let base_attributes =
    [ a_input_type input_type; a_value value; a_class [ "input" ] ]
  in
  let attributes =
    match name with
    | Some name -> base_attributes @ [ a_name name; a_placeholder input_label ]
    | None -> base_attributes
  in
  match input_type with
  | `Hidden -> input ~a:attributes ()
  | _ ->
    div
      ~a:[ a_class [ "flex-box"; "flex--column" ] ]
      [ label ~a:[ a_class [ "label" ] ] [ txt input_label ]
      ; input ~a:attributes ()
      ]
;;

let textarea_element
    language
    name
    input_label
    value
    ?(classnames = [])
    ?(attributes = [])
    ()
  =
  let input_label = Pool_common.Utils.field_to_string language input_label in
  let input =
    textarea
      ~a:([ a_name name; a_class ([ "input" ] @ classnames) ] @ attributes)
      (txt value)
  in
  div
    ~a:[ a_class [ "flex-box"; "flex--column" ] ]
    [ label ~a:[ a_class [ "label" ] ] [ txt input_label ]; input ]
;;

let submit_element lang submit ?(classnames = []) () =
  input
    ~a:
      [ a_input_type `Submit
      ; a_value (Pool_common.Utils.control_to_string lang submit)
      ; a_class ([ "button" ] @ classnames)
      ]
    ()
;;

let hx_input_element
    input_type
    name
    value
    (* TODO [aerben] Do we generally want HTMX input fields to be versioned? *)
      version
    input_label
    language
    ?hx_post
    ?(classnames = [])
    ?error
    ()
  =
  let field_to_string = Pool_common.Utils.field_to_string language in
  let attributes =
    (match input_type with
    | `Checkbox ->
      if bool_of_string_opt value |> CCOption.get_or ~default:false
      then [ a_checked () ]
      else []
    | _ -> [ a_value value; a_placeholder (field_to_string input_label) ])
    @ [ a_input_type input_type
      ; a_name name
      ; a_class ([ "input" ] @ classnames)
      ]
    @ (if not (CCList.is_empty classnames) then [ a_class classnames ] else [])
    @ htmx_attributes name version ?action:hx_post ()
  in
  let error_message error =
    span
      ~a:[ a_class [ "error-message" ] ]
      [ txt (error |> Pool_common.(Utils.error_to_string language)) ]
  in
  let field_content =
    [ label ~a:[ a_class [ "label" ] ] [ txt (field_to_string input_label) ]
    ; input ~a:attributes ()
    ]
  in
  let field_content =
    error
    |> CCOption.map_or ~default:field_content (fun error ->
           field_content @ [ error_message error ])
  in
  div
    ~a:[ a_class [ "flex-box"; "flex--column" ]; a_user_data "name" name ]
    field_content
;;
