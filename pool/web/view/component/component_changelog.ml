open Tyxml.Html
open Changelog

let yojson_to_string (json : Yojson.Safe.t) =
  match json with
  | `Bool bool -> Utils.Bool.to_string bool
  | `Int int -> CCInt.to_string int
  | `Float float -> CCFloat.to_string float
  | `String s -> s
  | _ -> Yojson.Safe.pretty_to_string json
;;

let rec format_changes changes =
  let open Changes in
  let format_change (key, value) =
    let changes = format_changes value in
    div
      ~a:[ a_class [ "flexrow"; "flex-gap" ] ]
      [ div ~a:[ a_class [ "changelog-key" ] ] [ txt key; txt ": " ]; changes ]
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
    let format json = span [ yojson_to_string json |> txt ] in
    span [ strong [ format before; txt " → "; format after ] ]
;;

let list Pool_context.{ language; _ } url changelog =
  let open Pool_common in
  let target_id = "changelog-datatable" in
  let field_to_string field =
    Utils.field_to_string_capitalized language field |> txt
  in
  match changelog with
  | None ->
    let trigger_onload =
      let url = url |> Uri.to_string |> Sihl.Web.externalize_path in
      [ a_id target_id; a_user_data "hx-trigger" "load" ]
      @ Data_table.hx_get ~url ~target_id ~push_url:false
    in
    div
      [ h2 [ field_to_string Pool_message.Field.History ]
      ; div ~a:trigger_onload []
      ]
  | Some (changelogs, query) ->
    let data_table =
      Data_table.create_meta ~push_url:false url query language
    in
    let cols =
      Pool_message.
        [ `custom (field_to_string Field.User)
        ; `custom (field_to_string Field.Changes)
        ; `custom (field_to_string Field.CreatedAt)
        ]
    in
    let th_class = [ "w-3"; "w-7"; "w-2" ] in
    let row ({ user_uuid; user_email; changes; created_at; _ } : t) =
      (* TODO: differ between admins and users, maybe create a route that
         redirects *)
      [ a
          ~a:
            [ a_href
                (Http_utils.Url.Admin.admin_path
                   ~id:(Admin.Id.of_common user_uuid)
                   ())
            ]
          [ txt (Pool_user.EmailAddress.value user_email) ]
      ; changes |> format_changes
      ; txt
          (created_at |> CreatedAt.value |> Pool_model.Time.formatted_date_time)
      ]
      |> CCList.map (fun value -> td [ value ])
      |> tr
    in
    Data_table.make
      ~align_top:true
      ~target_id
      ~th_class
      ~cols
      ~row
      data_table
      changelogs
;;
