open Tyxml.Html
open Changelog

let rec format_changes changes =
  let open Changes in
  let format_change (key, value) =
    let changes = format_changes value in
    div
      ~a:[ a_class [ "flexrow"; "flex-gap" ] ]
      [ div [ txt key; txt " : " ]; changes ]
  in
  let rec format_assoc_list acc = function
    | [] -> acc
    | hd :: tl ->
      let acc = acc @ [ format_change hd ] in
      format_assoc_list acc tl
  in
  match changes with
  | Assoc assocs -> format_assoc_list [] assocs |> div
  | Change (before, after) ->
    let format json = span [ Yojson.Safe.pretty_to_string json |> txt ] in
    span [ format before; txt " → "; format after ]
;;

let list Pool_context.{ language; _ } url (changelogs, query) =
  let open Pool_common in
  let field_to_string field =
    Utils.field_to_string_capitalized language field |> txt
  in
  let data_table = Data_table.create_meta ~push_url:false url query language in
  let cols =
    Pool_message.
      [ `custom (field_to_string Field.User)
      ; `custom (field_to_string Field.Changes)
      ; `custom (field_to_string Field.CreatedAt)
      ]
  in
  let th_class = [ "w-3"; "w-7"; "w-2" ] in
  let row ({ user_uuid; changes; created_at; _ } : t) =
    [ Id.value user_uuid |> txt
    ; changes |> format_changes
    ; txt (created_at |> CreatedAt.value |> Pool_model.Time.formatted_date_time)
    ]
    |> CCList.map (fun value -> td [ value ])
    |> tr
  in
  Data_table.make
    ~target_id:"location-changelog"
    ~th_class
    ~cols
    ~row
    data_table
    changelogs
;;
