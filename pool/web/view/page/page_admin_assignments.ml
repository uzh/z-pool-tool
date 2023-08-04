open CCFun.Infix
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
    Session.(session.id |> Id.value)
    Message.Field.(human_url Assignments)
    Assignment.(assignment.id |> Id.value)
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

  (* TODO[timhub]: replace with icon when it is added to framework *)
  let boolean_value = function
    | false -> "x" |> txt
    | true -> "✓" |> txt
  ;;

  let assignment_id { Assignment.id; _ } = id |> Assignment.Id.value |> txt

  let assignment_participated { Assignment.participated; _ } =
    participated
    |> CCOption.map_or ~default:(txt "") (Participated.value %> boolean_value)
  ;;

  let assignment_no_show { Assignment.no_show; _ } =
    no_show |> CCOption.map_or ~default:(txt "") (NoShow.value %> boolean_value)
  ;;

  let assignment_external_data_id { Assignment.external_data_id; _ } =
    external_data_id |> CCOption.map_or ~default:"" ExternalDataId.value |> txt
  ;;

  let contact_firstname ({ Assignment.contact; _ } : Assignment.t) =
    contact |> Contact.firstname |> Pool_user.Firstname.value |> txt
  ;;

  let contact_lastname ({ Assignment.contact; _ } : Assignment.t) =
    contact |> Contact.lastname |> Pool_user.Lastname.value |> txt
  ;;

  let contact_email ({ Assignment.contact; _ } : Assignment.t) =
    contact |> Contact.email_address |> Pool_user.EmailAddress.value |> txt
  ;;

  let contact_cellphone ({ Assignment.contact; _ } : Assignment.t) =
    contact
    |> fun { Contact.cell_phone; _ } ->
    CCOption.map_or ~default:"" Pool_user.CellPhone.value cell_phone |> txt
  ;;

  let canceled_at ({ Assignment.canceled_at; _ } : Assignment.t) =
    canceled_at
    |> CCOption.map_or
         ~default:""
         (Assignment.CanceledAt.value
          %> Pool_common.Utils.Time.formatted_date_time)
    |> txt
  ;;

  let overview_list
    ?(view_contact_name = false)
    ?(view_contact_email = false)
    ?(view_contact_cellphone = false)
    redirect
    Pool_context.{ language; csrf; _ }
    experiment_id
    session
    assignments
    =
    let default = txt "" in
    let deletable = CCFun.(Assignment.is_deletable %> CCResult.is_ok) in
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
        I18n.(
          if session.Session.has_follow_ups
          then CancelAssignmentWithFollowUps
          else CancelAssignment)
        (Message.Cancel None)
        Component.Icon.CloseCircle
    in
    let mark_as_deleted =
      let open Pool_common in
      button_form
        "mark-as-deleted"
        I18n.(
          if session.Session.has_follow_ups
          then MarkAssignmentWithFollowUpsAsDeleted
          else MarkAssignmentAsDeleted)
        Message.MarkAsDeleted
        Component.Icon.Trash
    in
    match CCList.is_empty assignments with
    | true -> p [ language |> empty ]
    | false ->
      let add_field_if check values = if check then values else [] in
      let contact_information =
        let open Pool_common.Message in
        add_field_if
          view_contact_name
          [ Field.Lastname, contact_lastname
          ; Field.Firstname, contact_firstname
          ]
        @ add_field_if view_contact_email [ Field.Email, contact_email ]
        @ add_field_if
            view_contact_cellphone
            [ Field.CellPhone, contact_cellphone ]
        |> function
        | [] -> [ Field.Id, assignment_id ]
        | fields -> fields
      in
      let thead =
        ((CCList.map fst contact_information
          @ Pool_common.Message.Field.
              [ Participated; NoShow; ExternalDataId; CanceledAt ])
         |> Component.Table.fields_to_txt language)
        @ [ default ]
      in
      let rows =
        let open CCFun in
        CCList.map
          (fun (assignment : Assignment.t) ->
            let base =
              CCList.map snd contact_information
              @ [ assignment_participated
                ; assignment_no_show
                ; assignment_external_data_id
                ; canceled_at
                ]
              |> CCList.map (fun fcn -> fcn assignment)
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

let list experiment ({ Pool_context.language; _ } as context) assignments =
  [ div
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
  ]
  |> Layout.Experiment.(
       create
         ~active_navigation:Pool_common.I18n.Assignments
         ~hint:Pool_common.I18n.ExperimentAssignment
         context
         (NavLink Pool_common.I18n.Assignments)
         experiment)
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
    div ~a:[ a_class [ "stack-lg" ] ] [ notification; list ] |> CCList.return
  in
  Layout.Experiment.(
    create
      ~active_navigation:Pool_common.I18n.Assignments
      ~hint:Pool_common.I18n.ExperimentAssignment
      context
      (I18n Pool_common.I18n.DeletedAssignments)
      experiment
      html)
;;
