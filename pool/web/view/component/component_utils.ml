open Tyxml.Html

let as_target_id = Format.asprintf "#%s"
let templates_disabled_key = "templates_disabled"

let format_identifiers ?prefix identifiers =
  let ids =
    (CCList.fold_left (fun str n ->
       if CCString.is_empty str
       then CCInt.to_string n
       else Format.asprintf "%s-%s" str (CCInt.to_string n)))
      ""
      identifiers
  in
  match prefix with
  | None -> ids
  | Some prefix -> Format.asprintf "%s-%s" prefix ids
;;

let htmx_attribs
  ~action
  ~trigger
  ?target
  ?(swap = "outerHTML")
  ?(allow_empty_values = false)
  ?templates_disabled
  ?identifier
  ()
  =
  let target = target |> CCOption.map (a_user_data "hx-target") in
  let hx_vals =
    let identifier =
      identifier
      |> CCOption.map (fun identifier -> "id", format_identifiers identifier)
    in
    let allow_empty_values =
      if allow_empty_values then Some ("allow_empty_values", "true") else None
    in
    let templates_disabled =
      templates_disabled
      |> CCOption.map (fun disabled ->
        templates_disabled_key, Bool.to_string disabled)
    in
    [ identifier; allow_empty_values; templates_disabled ]
    |> CCList.filter_map CCFun.id
    |> fun values ->
    if CCList.is_empty values
    then []
    else
      values
      |> CCList.map (fun (key, value) ->
        Format.asprintf "\"%s\": \"%s\"" key value)
      |> CCString.concat ","
      |> fun values -> [ a_user_data "hx-vals" (Format.asprintf "{%s}" values) ]
  in
  [ a_user_data "hx-post" (action |> Sihl.Web.externalize_path)
  ; a_user_data "hx-trigger" trigger
  ; a_user_data "hx-swap" swap
  ]
  @ hx_vals
  @ CCList.filter_map CCFun.id [ target ]
;;

let format_reminder_sent_opt ?(default = "") value =
  let open Pool_common in
  value
  |> CCOption.map_or
       ~default
       CCFun.(Reminder.SentAt.value %> Pool_model.Time.formatted_date_time)
  |> txt
;;
