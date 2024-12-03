open Tyxml.Html
open Component.Input
open Pool_message.Control

let role_assignment
      ?hint
      ?(can_assign = false)
      ?(can_unassign = false)
      base_url
      field
      { Pool_context.language; csrf; _ }
      ~assign
      ~unassign
      ~applicable:available
      ~current:existing
  =
  let open CCFun in
  let column ?hint title lst =
    let html =
      match hint, CCList.is_empty lst with
      | Some hint, true -> p [ txt (Pool_common.Utils.text_to_string language hint) ]
      | _ -> Component.Table.horizontal_table ~align_last_end:true `Striped lst
    in
    div
      [ h3
          ~a:[ a_class [ "heading-3" ] ]
          [ Pool_common.Utils.text_to_string language title |> txt ]
      ; html
      ]
  in
  let form action admin =
    let show, url, control, style =
      match action with
      | `Assign -> can_assign, assign, Assign None, `Success
      | `Unassign -> can_unassign, unassign, Unassign None, `Error
    in
    let button =
      if show
      then
        [ form
            ~a:[ a_action (Format.asprintf "%s/%s" (base_url admin) url); a_method `Post ]
            [ csrf_element csrf ()
            ; submit_element
                ~submit_type:style
                ~classnames:[ "small" ]
                language
                control
                ()
            ]
        ]
      else []
    in
    [ admin |> Admin.email_address |> Pool_user.EmailAddress.value |> txt
    ; admin |> Admin.fullname |> txt
    ]
    @ button
  in
  let open Pool_common.I18n in
  let existing =
    existing
    |> CCList.map (form `Unassign)
    |> column ~hint:(RoleCurrentlyNoneAssigned field) RoleCurrentlyAssigned
  in
  let available =
    available |> CCList.map (form `Assign) |> column RoleApplicableToAssign
  in
  let main_hint =
    CCOption.map_or
      ~default:(txt "")
      (I18n.content_to_string %> Unsafe.data %> CCList.return %> div)
      hint
  in
  div
    ~a:[ a_class [ "stack-lg" ] ]
    [ main_hint; div ~a:[ a_class [ "switcher"; "flex-gap" ] ] [ existing; available ] ]
;;
