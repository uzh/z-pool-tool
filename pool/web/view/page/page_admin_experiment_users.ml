open Tyxml.Html
open Component.Input

let role_assignment
  base_url
  field
  Pool_context.{ language; csrf; _ }
  ~assign
  ~unassign
  ~applicable:available
  ~current:existing
  =
  let column ?hint title lst =
    let html =
      match hint, CCList.is_empty lst with
      | Some hint, true ->
        p [ txt (Pool_common.Utils.text_to_string language hint) ]
      | _ -> Component.Table.horizontal_table ~align_last_end:true `Striped lst
    in
    div
      [ h3
          ~a:[ a_class [ "heading-3" ] ]
          [ Pool_common.Utils.text_to_string language title |> txt ]
      ; html
      ]
  in
  let open Pool_common in
  let open I18n in
  let form action admin =
    let url, control, style =
      match action with
      | `Assign -> assign, Message.(Assign None), `Success
      | `Unassign -> unassign, Message.(Unassign None), `Error
    in
    let delete =
      form
        ~a:
          [ a_action (Format.asprintf "%s/%s" (base_url admin) url)
          ; a_method `Post
          ]
        [ csrf_element csrf ()
        ; submit_element
            ~submit_type:style
            ~classnames:[ "small" ]
            language
            control
            ()
        ]
    in
    [ admin |> Admin.email |> txt; admin |> Admin.full_name |> txt; delete ]
  in
  let existing =
    existing
    |> CCList.map (form `Unassign)
    |> column ~hint:(RoleCurrentlyNoneAssigned field) RoleCurrentlyAssigned
  in
  let available =
    available |> CCList.map (form `Assign) |> column RoleApplicableToAssign
  in
  div ~a:[ a_class [ "switcher"; "flex-gap" ] ] [ existing; available ]
;;
