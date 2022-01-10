open Tyxml.Html

let admin_row admin =
  let open Sihl.Contract.User in
  let user = Admin.Any.user admin in
  div
    [ a
        ~a:
          [ a_href
              (Sihl.Web.externalize_path
                 (Format.asprintf "/admin/admins/%s" user.id))
          ]
        [ p [ txt user.email ] ]
    ]
;;

let list_admins admins message () =
  let user_rows = CCList.map (fun admin -> admin_row admin) admins in
  let html = div [ h1 [ txt "Admins" ]; div user_rows ] in
  Page_layout.create html message ()
;;

let show_admin roles_authorized_to_edit admin message () =
  let open Sihl.Contract.User in
  let user = Admin.Any.user admin in
  let edit_link =
    match CCList.mem (Admin.Any.role admin) roles_authorized_to_edit with
    | true ->
      [ a
          ~a:[ a_href (Format.asprintf "/admin/admins/%s/edit" user.id) ]
          [ txt "edit" ]
      ]
    | false -> []
  in
  let html =
    div
      [ h1
          [ txt
              (Format.asprintf
                 "%s %s"
                 (user.given_name |> Option.value ~default:"")
                 (user.name |> Option.value ~default:""))
          ]
      ; div edit_link
      ]
  in
  Page_layout.create html message ()
;;

let edit_admin _ editabe_admin message () =
  let open Sihl.Contract.User in
  let user = Admin.Any.user editabe_admin in
  let html =
    div
      [ h1
          [ txt
              (Format.asprintf
                 "%s %s"
                 (user.given_name |> Option.value ~default:"")
                 (user.name |> Option.value ~default:""))
          ]
      ]
  in
  Page_layout.create html message ()
;;

let participant_row participant =
  let open Participant in
  div
    [ a
        ~a:
          [ a_href
              (Sihl.Web.externalize_path
                 (Format.asprintf
                    "/admin/participants/%s"
                    (id participant |> Pool_common.Id.value)))
          ]
        [ p [ txt (email_address participant |> Pool_user.EmailAddress.value) ]
        ]
    ]
;;

let list_participants participants message () =
  let participant_rows =
    CCList.map (fun participant -> participant_row participant) participants
  in
  let html = div [ h1 [ txt "Participants" ]; div participant_rows ] in
  Page_layout.create html message ()
;;

let show_participant participant message () =
  let html = div [ h1 [ txt (Participant.fullname participant) ] ] in
  Page_layout.create html message ()
;;
