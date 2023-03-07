open Tyxml.Html
open Component.Input

let assignments_path experiment_id =
  Format.asprintf
    "/admin/experiments/%s/assignments"
    (experiment_id |> Experiment.Id.value)
;;

let assignment_specific_path experiment_id session assignment =
  let open Pool_common in
  Format.asprintf
    "/admin/experiments/%s/sessions/%s/%s/%s/%s"
    (experiment_id |> Experiment.Id.value)
    (session.Session.id |> Id.value)
    Message.Field.(human_url Assignments)
    (assignment.Assignment.id |> Id.value)
;;

type assignment_redirect =
  | Assignments
  | DeletedAssignments
  | Session
[@@deriving show { with_path = false }, yojson]

let read_assignment_redirect m =
  m
  |> Format.asprintf "[\"%s\"]"
  |> Yojson.Safe.from_string
  |> fun json ->
  try Some (assignment_redirect_of_yojson json) with
  | _ -> None
;;

module Partials = struct
  open Assignment

  let empty language =
    Pool_common.(
      Utils.text_to_string language (I18n.EmtpyList Message.Field.Assignments))
    |> txt
  ;;

  let contact_fullname (a : Assignment.t) = a.contact |> Contact.fullname |> txt

  let contact_email (a : Assignment.t) =
    a.contact |> Contact.email_address |> Pool_user.EmailAddress.value |> txt
  ;;

  let canceled_at (a : Assignment.t) =
    a.canceled_at
    |> CCOption.map_or ~default:"" (fun c ->
         c
         |> Assignment.CanceledAt.value
         |> Pool_common.Utils.Time.formatted_date_time)
    |> txt
  ;;

  let overview_list
    redirect
    Pool_context.{ language; csrf; _ }
    experiment_id
    session
    assignments
    =
    let deletable m = m |> Assignment.is_deletable |> CCResult.is_ok in
    let cancelable m =
      Session.assignments_cancelable session |> CCResult.is_ok
      && Assignment.is_cancellable m |> CCResult.is_ok
    in
    let action assignment suffix =
      assignment_specific_path experiment_id session assignment suffix
      |> Sihl.Web.externalize_path
    in
    let button_form suffix confirmable control icon assignment =
      let hidden_redirect_input =
        let open Pool_common.Message in
        input_element
          ~value:(show_assignment_redirect redirect)
          language
          `Hidden
          Field.Redirect
      in
      form
        ~a:
          [ a_action (action assignment suffix)
          ; a_method `Post
          ; a_user_data
              "confirmable"
              Pool_common.(Utils.confirmable_to_string language confirmable)
          ]
        [ csrf_element csrf ()
        ; hidden_redirect_input
        ; submit_element language control ~submit_type:`Error ~has_icon:icon ()
        ]
    in
    let cancel =
      let open Pool_common in
      button_form
        "cancel"
        I18n.CancelAssignment
        (Message.Cancel None)
        `CloseCircle
    in
    let mark_as_deleted =
      let open Pool_common in
      (* TODO[timhub]: Add hint, only if this assignment has a follow-up
         assignment? Or only if this session has follow-up session *)
      button_form
        "mark-as-deleted"
        I18n.MarkAssignmentAsDeleted
        Message.MarkAsDeleted
        `Trash
    in
    match CCList.is_empty assignments with
    | true -> p [ language |> empty ]
    | false ->
      let thead =
        (Pool_common.Message.Field.[ Name; Email; CanceledAt ]
         |> Component.Table.fields_to_txt language)
        @ [ txt "" ]
      in
      let rows =
        CCList.map
          (fun (assignment : Assignment.t) ->
            let base =
              [ assignment |> contact_fullname
              ; assignment |> contact_email
              ; assignment |> canceled_at
              ]
            in
            let buttons =
              [ cancelable assignment, cancel
              ; deletable assignment, mark_as_deleted
              ]
              |> CCList.filter_map (fun (active, form) ->
                   if not active then None else Some (form assignment))
              |> div ~a:[ a_class [ "flexrow"; "flex-gap"; "inline-flex" ] ]
              |> CCList.pure
            in
            base @ buttons)
          assignments
      in
      Component.Table.horizontal_table `Striped ~align_last_end:true ~thead rows
  ;;

  let grouped_overview_lists
    redirect
    (Pool_context.{ language; _ } as context)
    experiment
    assignments
    =
    CCList.map
      (fun (session, assignments) ->
        let attrs, to_title =
          if CCOption.is_some session.Session.follow_up_to
          then
            ( [ a_class [ "inset"; "left" ] ]
            , fun session ->
                Format.asprintf
                  "%s (%s)"
                  (session |> Session.session_date_to_human)
                  (Pool_common.Utils.field_to_string
                     language
                     Field.FollowUpSession) )
          else [], Session.session_date_to_human
        in
        div
          ~a:attrs
          [ h3 ~a:[ a_class [ "heading-3" ] ] [ txt (session |> to_title) ]
          ; overview_list
              redirect
              context
              experiment.Experiment.id
              session
              assignments
          ])
      assignments
    |> div ~a:[ a_class [ "stack-lg" ] ]
  ;;
end

let list experiment (Pool_context.{ language; _ } as context) assignments =
  let html =
    div
      [ p
          [ a
              ~a:
                [ a_href
                    (assignments_path experiment.Experiment.id
                     |> Format.asprintf "%s/deleted"
                     |> Sihl.Web.externalize_path)
                ]
              [ txt
                  Pool_common.(
                    Utils.text_to_string language I18n.DeletedAssignments)
              ]
          ]
      ; Partials.grouped_overview_lists
          Assignments
          context
          experiment
          assignments
      ]
  in
  Page_admin_experiments.experiment_layout
    ~hint:Pool_common.I18n.ExperimentAssignment
    language
    (Page_admin_experiments.NavLink Pool_common.I18n.Assignments)
    experiment
    ~active:Pool_common.I18n.Assignments
    html
;;

let marked_as_deleted
  experiment
  (Pool_context.{ language; _ } as context)
  assignments
  =
  let html =
    let notification =
      let open Pool_common in
      [ I18n.AssignmentsMarkedAsClosed |> Utils.hint_to_string language |> txt ]
      |> div
      |> CCList.pure
      |> Component.Notification.notification language `Warning
    in
    let list =
      Partials.grouped_overview_lists
        DeletedAssignments
        context
        experiment
        assignments
    in
    div ~a:[ a_class [ "stack-lg" ] ] [ notification; list ]
  in
  Page_admin_experiments.experiment_layout
    ~hint:Pool_common.I18n.ExperimentAssignment
    language
    (Page_admin_experiments.I18n Pool_common.I18n.DeletedAssignments)
    experiment
    html
;;
