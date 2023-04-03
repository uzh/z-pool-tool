open Entity
open Tyxml.Html
module Field = Pool_common.Message.Field

let read_entity entity =
  Guard.(ValidationSet.One (Action.Read, TargetSpec.Entity entity))
;;

type title =
  | Control of Pool_common.Message.control
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

let nav_elements { Experiment.id; _ } =
  let open Guard in
  let open ValidationSet in
  let open I18n in
  let target_id = id |> Uuid.target_of Experiment.Id.value in
  [ "", Overview, Experiment.Guard.Access.read id
  ; ( "assistants"
    , Field Field.Assistants
    , Or
        [ SpecificRole `ManageAssistants
        ; SpecificRole (`ManageAssistant target_id)
        ] )
  ; ( "experimenter"
    , Field Field.Experimenter
    , Or
        [ SpecificRole `ManageExperimenters
        ; SpecificRole (`ManageExperimenter target_id)
        ] )
  ; "invitations", Invitations, Invitation.Guard.Access.index id
  ; "waiting-list", WaitingList, Waiting_list.Guard.Access.index id
  ; "sessions", Sessions, Session.Guard.Access.index id
  ; "assignments", Assignments, Assignment.Guard.Access.index id
  ; "mailings", Mailings, Mailing.Guard.Access.index id
  ]
  |> CCList.map (fun (url, label, set) ->
       ( Format.asprintf "/admin/experiments/%s/%s" (Experiment.Id.value id) url
       , label
       , set ))
  |> NavElement.create_all_req_with_set
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

let with_heading ({ Experiment.title; _ } : Experiment.t) children =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt Experiment.(title |> Title.value) ]
    ; children
    ]
;;

let create
  ?active_navigation
  ?buttons
  ?hint
  { Pool_context.database_label; language; user; _ }
  title
  experiment
  content
  =
  let open Utils.Lwt_result.Infix in
  let%lwt actor =
    Pool_context.Utils.find_authorizable_opt database_label user
  in
  let html = combine ?buttons ?hint language title content in
  let%lwt subpage =
    nav_elements experiment
    |> Navigation_utils.filter_items ~validate:true ?actor database_label
    ||> CCFun.flip (Navigation_tab.create ?active_navigation language) html
  in
  with_heading experiment subpage |> Lwt.return
;;
