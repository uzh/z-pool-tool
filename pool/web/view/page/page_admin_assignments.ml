open CCFun.Infix
open Tyxml.Html
open Component.Input
module Field = Pool_common.Message.Field

let assignments_path experiment_id =
  Format.asprintf
    "/admin/experiments/%s/assignments"
    (experiment_id |> Experiment.Id.value)
;;

let assignment_specific_path ?suffix experiment_id session_id assignment_id =
  let open Pool_common in
  let base =
    Format.asprintf
      "/admin/experiments/%s/sessions/%s/%s/%s"
      (experiment_id |> Experiment.Id.value)
      Session.(session_id |> Id.value)
      Message.Field.(human_url Assignments)
      Assignment.(assignment_id |> Id.value)
  in
  suffix |> CCOption.map_or ~default:base (Format.asprintf "%s/%s" base)
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

  let identity view_contact_name contact id =
    if view_contact_name then Contact.fullname contact else Id.value id
  ;;

  let overview_list
    ?(view_contact_name = false)
    ?(view_contact_email = false)
    ?(view_contact_cellphone = false)
    ?(external_data_required = false)
    redirect
    Pool_context.{ language; csrf; _ }
    experiment_id
    session
    assignments
    =
    let open Pool_common in
    let default = txt "" in
    let deletable = CCFun.(Assignment.is_deletable %> CCResult.is_ok) in
    let cancelable m =
      Session.assignments_cancelable session |> CCResult.is_ok
      && Assignment.is_cancellable m |> CCResult.is_ok
    in
    let action { Assignment.id; _ } suffix =
      assignment_specific_path ~suffix experiment_id session.Session.id id
      |> Sihl.Web.externalize_path
    in
    let button_form
      ?title
      ?(style = `Primary)
      suffix
      confirmable
      control
      icon
      assignment
      =
      let hidden_redirect_input =
        input_element
          ~value:(show_assignment_redirect redirect)
          language
          `Hidden
          Field.Redirect
      in
      let icon = Icon.to_html icon in
      let submit_content =
        match control with
        | None -> [ icon ]
        | Some control ->
          [ icon
          ; span [ txt Pool_common.Utils.(control_to_string language control) ]
          ]
      in
      let classnames = submit_type_to_class style :: [ "has-icon" ] in
      let title =
        CCOption.map (fun title -> title language) title
        |> function
        | None -> []
        | Some title -> [ a_title title ]
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
        ; button
            ~a:([ a_button_type `Submit; a_class classnames ] @ title)
            submit_content
        ]
    in
    let edit m =
      let action = action m "edit" in
      link_as_button action ~icon:Component.Icon.Create
    in
    let cancel =
      button_form
        ~style:`Error
        "cancel"
        I18n.(
          if session.Session.has_follow_ups
          then CancelAssignmentWithFollowUps
          else CancelAssignment)
        (Some (Message.Cancel None))
        Component.Icon.CloseCircle
    in
    let mark_as_deleted =
      let title language =
        Utils.control_to_string language Message.MarkAsDeleted
      in
      button_form
        ~title
        ~style:`Error
        "mark-as-deleted"
        I18n.(
          if session.Session.has_follow_ups
          then MarkAssignmentWithFollowUpsAsDeleted
          else MarkAssignmentAsDeleted)
        None
        Component.Icon.Trash
    in
    match CCList.is_empty assignments with
    | true -> p [ language |> empty ]
    | false ->
      let add_field_if check values = if check then values else [] in
      let contact_information =
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
      let external_data_field =
        add_field_if
          external_data_required
          [ Field.ExternalDataId, assignment_external_data_id ]
      in
      let thead =
        let field_to_text = Component.Table.field_to_txt language in
        let left =
          CCList.map (fun (field, _) -> field_to_text field) contact_information
          @ CCList.map
              (fun (field, _) -> field_to_text field)
              external_data_field
        in
        let checkboxes = [ txt "P"; txt "NS" ] in
        let right = [ Field.CanceledAt |> field_to_text; default ] in
        left @ checkboxes @ right
      in
      let rows =
        let open CCFun in
        CCList.map
          (fun (assignment : Assignment.t) ->
            let base =
              CCList.map snd contact_information
              @ CCList.map snd external_data_field
              @ [ assignment_participated; assignment_no_show; canceled_at ]
              |> CCList.map (fun fcn -> fcn assignment)
            in
            let buttons =
              [ true, edit
              ; cancelable assignment, cancel
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
      div
        [ p
            [ Utils.hint_to_string language I18n.SessionCloseLegend
              |> HttpUtils.add_line_breaks
            ]
        ; Component.Table.horizontal_table
            `Striped
            ~align_last_end:true
            ~thead
            rows
        ]
  ;;

  let grouped_overview_lists
    ?view_contact_name
    ?view_contact_email
    ?view_contact_cellphone
    redirect
    (Pool_context.{ language; _ } as context)
    { Experiment.id; external_data_required; _ }
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
              ?view_contact_name
              ?view_contact_email
              ?view_contact_cellphone
              ~external_data_required:
                (Experiment.ExternalDataRequired.value external_data_required)
              redirect
              context
              id
              session
              assignments
          ])
      assignments
    |> div ~a:[ a_class [ "stack-lg" ] ]
  ;;
end

let list
  ?view_contact_name
  ?view_contact_email
  ?view_contact_cellphone
  ({ Experiment.id; _ } as experiment)
  ({ Pool_context.language; _ } as context)
  assignments
  =
  [ div
      [ p
          [ a
              ~a:
                [ a_href
                    (assignments_path id
                     |> Format.asprintf "%s/deleted"
                     |> Sihl.Web.externalize_path)
                ]
              [ txt
                  Pool_common.(
                    Utils.text_to_string language I18n.DeletedAssignments)
              ]
          ]
      ; Partials.grouped_overview_lists
          ?view_contact_name
          ?view_contact_email
          ?view_contact_cellphone
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
  ?view_contact_name
  ?view_contact_email
  ?view_contact_cellphone
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
        ?view_contact_name
        ?view_contact_email
        ?view_contact_cellphone
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

let edit
  ({ Pool_context.language; csrf; _ } as context)
  view_contact_name
  experiment
  session
  { Assignment.id; no_show; participated; external_data_id; contact; _ }
  =
  let open Assignment in
  let open Component.Input in
  let open CCOption.Infix in
  let action =
    assignment_specific_path experiment.Experiment.id session.Session.id id
  in
  let session_data =
    let open Session in
    let field_to_string = Pool_common.Utils.field_to_string language in
    div
      ~a:[ a_class [ "stack"; "inset"; "border"; " bg-grey-light" ] ]
      [ p
          [ txt (field_to_string Field.Start |> CCString.capitalize_ascii)
          ; txt ": "
          ; session.start
            |> Start.value
            |> Pool_common.Utils.Time.formatted_date_time
            |> txt
          ]
      ; Component.Location.preview session.location
      ]
  in
  [ div
      ~a:[ a_class [ "switcher"; "flex-gap" ] ]
      [ form
          ~a:[ a_action action; a_method `Post ]
          [ csrf_element csrf ()
          ; div
              ~a:[ a_class [ "flexcolumn"; "stack" ] ]
              [ checkbox_element
                  ?value:(no_show >|= NoShow.value)
                  language
                  Field.NoShow
              ; checkbox_element
                  ?value:(participated >|= Participated.value)
                  language
                  Field.Participated
              ; input_element
                  ?value:(external_data_id >|= ExternalDataId.value)
                  language
                  `Text
                  Field.ExternalDataId
              ; submit_element
                  language
                  ~classnames:[ "align-self-end" ]
                  Pool_common.Message.(Save None)
                  ()
              ]
          ]
      ; session_data
      ]
  ]
  |> Layout.Experiment.(
       create
         context
         (Text (Partials.identity view_contact_name contact id))
         experiment)
;;
