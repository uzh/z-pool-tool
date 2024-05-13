open Entity
module NavUtils = Navigation_utils
module Field = Pool_message.Field

let read_entity entity =
  Guard.(ValidationSet.one_of_tuple (Permission.Read, entity, None))
;;

let experiment_url id =
  Format.asprintf "/admin/experiments/%s/%s" (Experiment.Id.value id)
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
  let url = experiment_url id in
  let sessions_label =
    if Experiment.is_sessionless experiment then TimeWindows else Sessions
  in
  let left =
    [ Single (url "", Overview, Set (Guard.Access.read id))
    ; Single
        (url "sessions", sessions_label, Set (Session.Guard.Access.index id))
    ; Parent
        ( None
        , Invitations
        , OnChildren
        , [ Single
              (url "invitations", Filter, Set (Invitation.Guard.Access.index id))
          ; Single
              (url "mailings", Mailings, Set (Mailing.Guard.Access.index id))
          ; Single
              ( url "invitations/sent"
              , SentInvitations
              , Set (Invitation.Guard.Access.index id) )
          ] )
    ]
  in
  let waiting_list_nav =
    if experiment
       |> direct_registration_disabled
       |> DirectRegistrationDisabled.value
    then
      [ Single
          ( url "waiting-list"
          , WaitingList
          , Set (Waiting_list.Guard.Access.index id) )
      ]
    else []
  in
  let right =
    [ Parent
        ( None
        , Settings
        , OnChildren
        , [ Single
              ( url "assistants"
              , Field Field.Assistants
              , Set (Assistant.read ~target_uuid ()) )
          ; Single
              ( url "experimenter"
              , Field Field.Experimenter
              , Set (Experimenter.read ~target_uuid ()) )
          ] )
    ; Single (url "messages", MessageHistory, Set Queue.Guard.Access.index)
    ]
  in
  left @ waiting_list_nav @ right |> CCList.map NavElement.create
;;

let create
  ?active_navigation
  ?buttons
  ?hint
  ({ Pool_context.database_label; language; user; _ } as context)
  title
  experiment
  content
  =
  let open Utils.Lwt_result.Infix in
  let open Tab_navigation in
  let title = title_to_string language title in
  let active_navigation =
    active_navigation |> CCOption.map (experiment_url experiment.Experiment.id)
  in
  let%lwt actor =
    Pool_context.Utils.find_authorizable database_label user
    ||> Pool_common.Utils.get_or_failwith
  in
  let html = make_body ?buttons ?hint language title content in
  let title = Experiment.(Title.value experiment.title) in
  let subpage =
    make_tabs
      ~actor
      ?active_navigation
      ~overlay_title:title
      context
      html
      (nav_elements experiment)
  in
  with_heading title subpage |> Lwt.return
;;
