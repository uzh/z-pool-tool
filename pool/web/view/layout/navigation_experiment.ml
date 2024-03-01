open Entity
open Tyxml.Html
module Field = Pool_message.Field

let read_entity entity =
  Guard.(ValidationSet.one_of_tuple (Permission.Read, entity, None))
;;

type title =
  | Control of Pool_message.Control.t
  | NavLink of I18n.nav_link
  | I18n of I18n.t
  | Text of string

let title_to_string language text =
  let open Pool_common.Utils in
  match text with
  | Control text -> control_to_string language text
  | NavLink text -> nav_link_to_string language text
  | I18n text -> text_to_string language text
  | Text str -> str
;;

let nav_elements experiment =
  let open Guard in
  let open Access.Role.Assignment in
  let open I18n in
  let open Experiment in
  let id = experiment |> id in
  let target_uuid = Uuid.target_of Id.value id in
  let left =
    [ "", Overview, Set (Guard.Access.read id)
    ; "assistants", Field Field.Assistants, Set (Assistant.read ~target_uuid ())
    ; ( "experimenter"
      , Field Field.Experimenter
      , Set (Experimenter.read ~target_uuid ()) )
    ; "invitations", Invitations, Set (Invitation.Guard.Access.index id)
    ]
  in
  let right =
    [ "sessions", Sessions, Set (Session.Guard.Access.index id)
    ; "mailings", Mailings, Set (Mailing.Guard.Access.index id)
    ; "messages", MessageHistory, Set Queue.Guard.Access.index
    ]
  in
  let waiting_list_nav =
    if experiment
       |> direct_registration_disabled
       |> DirectRegistrationDisabled.value
    then
      [ "waiting-list", WaitingList, Set (Waiting_list.Guard.Access.index id) ]
    else []
  in
  left @ waiting_list_nav @ right
  |> CCList.map (fun (url, label, set) ->
    Single
      (Format.asprintf "/admin/experiments/%s/%s" (Id.value id) url, label, set)
    |> NavElement.create)
;;

let combine ?buttons ?hint language title children =
  let title =
    let base =
      h2 ~a:[ a_class [ "heading-2" ] ] [ txt (title_to_string language title) ]
    in
    let title =
      let classnames =
        [ "flexrow"; "justify-between"; "flex-gap"; "flexcolumn-mobile" ]
      in
      CCOption.map_or
        ~default:base
        (fun buttons ->
          div ~a:[ a_class classnames ] [ div [ base ]; div [ buttons ] ])
        buttons
    in
    CCOption.map_or
      ~default:[ title ]
      (fun hint ->
        [ title
        ; p
            [ Pool_common.Utils.hint_to_string language hint
              |> Http_utils.add_line_breaks
            ]
        ])
      hint
  in
  title @ [ div ~a:[ a_class [ "gap-lg" ] ] children ]
;;

let with_heading experiment children =
  let open Experiment in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ experiment |> title |> Title.value |> txt ]
    ; children
    ]
;;

let create
  ?active_navigation
  ?buttons
  ?hint
  { Pool_context.database_label; language; user; guardian; _ }
  title
  experiment
  content
  =
  let%lwt actor =
    Pool_context.Utils.find_authorizable_opt database_label user
  in
  let html = combine ?buttons ?hint language title content in
  let subpage =
    nav_elements experiment
    |> Navigation_utils.filter_items ~validate:true ?actor ~guardian
    |> CCFun.flip (Navigation_tab.create ?active_navigation language) html
  in
  with_heading experiment subpage |> Lwt.return
;;
