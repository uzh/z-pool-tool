open Containers
open CCFun
open Tyxml.Html
module DataTable = Component.DataTable
module Table = Component.Table
module Input = Component.Input
module Icon = Component.Icon
module Modal = Component.Modal
module Status = Component.UserStatus.Contact
module Duplicates = Page_admin_contact_duplicates

let path = Contact.id %> Contact.Id.value %> Format.asprintf "/admin/contacts/%s"
let enroll_contact_modal_id = "enroll-modal"

let enroll_contact_path ?suffix contact_id =
  Format.asprintf
    "/admin/contacts/%s/%s"
    (Contact.Id.value contact_id)
    Pool_message.Field.(Experiments |> human_url)
  |> (fun base ->
  match suffix with
  | None -> base
  | Some suffix -> Format.asprintf "%s/%s" base suffix)
  |> Sihl.Web.externalize_path
;;

let heading_with_icons contact =
  h1
    ~a:[ a_class [ "heading-1" ] ]
    [ Status.identity_with_icons Pool_common.Language.En ~context:`All true contact ]
;;

let personal_detail ?admin_comment ?custom_fields ?tags current_user language contact =
  let open Contact in
  let open Pool_message in
  let field_to_string =
    CCFun.(Pool_common.Utils.field_to_string language %> CCString.capitalize_ascii)
  in
  let tags =
    tags
    |> CCOption.map_or ~default:[] (fun tags ->
      [ field_to_string Field.Tags, Component.Tag.tag_list language tags ])
  in
  let custom_field_rows =
    let open Custom_field in
    let open CCList in
    let field_to_row custom_field =
      let field_name = Public.name_value language custom_field in
      tr
        [ th [ txt field_name ]
        ; td [ Component.CustomField.answer_to_html current_user language custom_field ]
        ]
    in
    custom_fields
    |> CCOption.map_or ~default:[] (fun (grouped, ungrouped) ->
      let ungrouped = ungrouped >|= field_to_row in
      let grouped =
        grouped |> flat_map (fun { Group.Public.fields; _ } -> fields >|= field_to_row)
      in
      ungrouped @ grouped)
  in
  let table_row (label, value) = tr [ th [ txt label ]; td [ value ] ] in
  let counter_rows =
    [ Field.InvitationCount, num_invitations %> NumberOfInvitations.value
    ; Field.AssignmentCount, num_assignments %> NumberOfAssignments.value
    ; Field.NoShowCount, num_no_shows %> NumberOfNoShows.value
    ; Field.ShowUpCount, num_show_ups %> NumberOfShowUps.value
    ; Field.Participated, num_participations %> NumberOfParticipations.value
    ]
    |> CCList.map (fun (field, value) ->
      field_to_string field, value contact |> string_of_int |> txt)
  in
  let info_rows =
    Pool_message.
      [ Field.Name, fullname contact |> txt
      ; Field.Email, email_address contact |> Pool_user.EmailAddress.value |> txt
      ; ( Field.CellPhone
        , contact.cell_phone
          |> CCOption.map_or ~default:"" Pool_user.CellPhone.value
          |> txt )
      ; ( Field.Language
        , contact.language |> CCOption.map_or ~default:"" Pool_common.Language.show |> txt
        )
      ; ( Field.TermsAndConditionsLastAccepted
        , contact.terms_accepted_at
          |> CCOption.map_or
               ~default:""
               (Pool_user.TermsAccepted.value %> Pool_model.Time.formatted_date_time)
          |> txt )
      ; ( Field.AdminComment
        , admin_comment
          |> CCOption.map_or
               ~default:(txt "")
               (AdminComment.value %> Http_utils.add_line_breaks) )
      ]
    |> CCList.map (fun (field, value) -> field_to_string field, value)
  in
  let rows =
    (info_rows @ tags |> CCList.map table_row)
    @ custom_field_rows
    @ (counter_rows |> CCList.map table_row)
  in
  table ~a:[ a_class (Table.table_classes `Striped ~align_top:true ()) ] rows
  |> fun html ->
  div
    [ h3
        ~a:[ a_class [ "heading-3" ] ]
        [ Pool_common.(Utils.nav_link_to_string language I18n.PersonalDetails |> txt) ]
    ; html
    ]
;;

let assign_contact_experiment_modal
      { Pool_context.language; csrf; _ }
      contact_id
      (experiment : Experiment.t)
      sessions
      matches_filter
  =
  let open Pool_common in
  let title language = Utils.text_to_string language I18n.EnrollInExperiment in
  let registration_disabled =
    Experiment.(experiment.registration_disabled |> RegistrationDisabled.value)
  in
  let notification =
    I18n.
      [ not matches_filter, ContactEnrollmentDoesNotMatchFilter
      ; registration_disabled, ContactEnrollmentRegistrationDisabled
      ]
    |> CCList.filter_map (fun (show, i18n) ->
      if show then Some (p [ Utils.hint_to_string language i18n |> txt ]) else None)
    |> function
    | [] -> txt ""
    | msg -> Component.Notification.notification language `Error msg
  in
  let session_select =
    let label = Session.start_end_with_duration_human in
    let follow_up_row session = div [ txt (label session) ] in
    let row (session, follow_ups) =
      let tooltip, attribs =
        Session.assignment_creatable session
        |> function
        | Ok () ->
          ( txt ""
          , [ a_name Pool_message.Field.(show Session)
            ; a_value Session.(Id.value session.id)
            ; a_required ()
            ] )
        | Error msg ->
          ( msg |> Utils.error_to_string language |> Component.Tooltip.create
          , [ a_disabled () ] )
      in
      div
        ~a:[ a_class [ "flexrow"; "flex-gap-xs" ] ]
        [ span [ input ~a:(a_input_type `Radio :: attribs) () ]
        ; div
            ~a:[ a_class [ "grow"; "flexcolumn"; "stack-xs" ] ]
            [ div
                ~a:[ a_class [ "flexrow"; "flex-gap-sm" ] ]
                [ span [ txt (label session) ]; tooltip ]
            ; div
                ~a:[ a_class [ "inset"; " left"; "hide-empty" ] ]
                (CCList.map follow_up_row follow_ups)
            ]
        ]
    in
    sessions |> CCList.map row |> div ~a:[ a_class [ "stack" ] ]
  in
  let form =
    div
      [ h4 [ txt Pool_message.Field.(show Sessions |> CCString.capitalize_ascii) ]
      ; form
          ~a:
            [ a_method `Post
            ; a_action
                (enroll_contact_path
                   ~suffix:Experiment.(Id.value experiment.Experiment.id)
                   contact_id)
            ; a_class [ "stack" ]
            ]
          Input.
            [ csrf_element csrf ()
            ; session_select
            ; submit_element language Pool_message.Control.Enroll ()
            ]
      ]
  in
  let html = div ~a:[ a_class [ "stack-lg" ] ] [ notification; form ] in
  Modal.create ~active:true language title enroll_contact_modal_id html
;;

let assign_contact_experiment_list { Pool_context.language; _ } contact_id experiments =
  let base_class = [ "data-item" ] in
  let disabled = [ a_class ([ "bg-grey-lightest"; "not-allowed" ] @ base_class) ] in
  let htmx_attribs experiment_id =
    [ a_user_data "hx-trigger" "click"
    ; a_user_data
        "hx-get"
        (enroll_contact_path ~suffix:Experiment.(Id.value experiment_id) contact_id)
    ; a_user_data "hx-swap" "outerHTML"
    ; a_user_data "hx-target" (Format.asprintf "#%s" enroll_contact_modal_id)
    ]
  in
  let assignable_attrs experiment_id = a_class base_class :: htmx_attribs experiment_id in
  let not_matching_filter experiment_id =
    [ a_class ([ "bg-red-lighter" ] @ base_class) ] @ htmx_attribs experiment_id
  in
  div
    ~a:[ a_class [ "data-list"; "relative"; "flexcolumn"; "active" ] ]
    (match experiments with
     | [] ->
       [ div
           ~a:[ a_class [ "inset-sm"; "cursor-default" ] ]
           [ txt Pool_common.(Utils.text_to_string language I18n.EmptyListGeneric) ]
       ]
     | experiments ->
       let open Experiment in
       let open DirectEnrollment in
       CCList.map
         (fun ({ id; title; public_title; matches_filter; _ } as experiment) ->
            let attrs =
              if not (assignable experiment)
              then disabled
              else if not matches_filter
              then not_matching_filter id
              else assignable_attrs id
            in
            div
              ~a:attrs
              [ txt
                  (Format.asprintf
                     "%s (%s)"
                     (Title.value title)
                     (PublicTitle.value public_title))
              ])
         experiments)
;;

let assign_contact_form { Pool_context.csrf; language; _ } contact =
  let open Pool_common in
  let form_identifier = Pool_common.Id.(create () |> value) in
  let legend =
    Component.Table.(
      table_legend
        Pool_common.Utils.
          [ ( error_to_string language Pool_message.Error.ContactDoesNotMatchFilter
            , legend_color_item "bg-red-lighter" )
          ])
  in
  let form =
    match Contact.is_inactive contact with
    | true ->
      p [ txt Utils.(error_to_string language Pool_message.Error.ContactIsInactive) ]
    | false ->
      div
        [ div
            ~a:[ a_class [ "stack" ] ]
            [ legend
            ; form
                ~a:[ a_id form_identifier ]
                [ Input.csrf_element csrf ()
                ; Component.Search.Experiment.assign_contact_search language contact ()
                ]
            ]
        ; div
            ~a:[ a_id enroll_contact_modal_id; a_class [ "modal"; "fullscreen-overlay" ] ]
            []
        ]
  in
  div
    [ h3
        ~a:[ a_class [ "has-gap" ] ]
        [ txt Utils.(text_to_string language I18n.EnrollInExperiment) ]
    ; form
    ]
;;

let list Pool_context.{ language; _ } contacts query =
  let url = Uri.of_string "/admin/contacts" in
  let data_table =
    Component.DataTable.create_meta
      ?filter:Contact.filterable_by
      ~search:Contact.searchable_by
      url
      query
      language
  in
  let cols = Pool_user.[ `column column_name; `column column_email; `empty ] in
  let th_class = [ "w-5"; "w-5"; "w-2" ] in
  let row (Contact.{ disabled; _ } as contact) =
    let a =
      if Pool_user.Disabled.value disabled then [ a_class [ "bg-red-lighter" ] ] else []
    in
    [ Status.identity_with_icons language true contact
    ; Status.email_with_icons language contact
    ; Input.link_as_button ~icon:Icon.Eye (path contact)
    ]
    |> CCList.map (CCList.return %> td)
    |> tr ~a
  in
  Component.DataTable.make
    ~target_id:"contacts-list"
    ~th_class
    ~cols
    ~row
    data_table
    contacts
;;

let index ({ Pool_context.language; _ } as context) contacts query =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1"; "has-gap" ] ]
        [ txt Pool_common.(Utils.nav_link_to_string language I18n.Contacts) ]
    ; div
        ~a:[ a_class [ "stack" ] ]
        [ Status.status_icons_table_legend language `All; list context contacts query ]
    ]
;;

let experiment_history_modal_id = "past-experiment-modal"

let experiment_history Pool_context.{ language; _ } contact experiments query =
  let url =
    contact
    |> Contact.id
    |> Contact.Id.value
    |> Format.asprintf "/admin/contacts/%s/past-experiments"
    |> Uri.of_string
  in
  let data_table =
    DataTable.create_meta
      ?filter:Experiment.filterable_by
      ~push_url:false
      ~search:Experiment.searchable_by
      url
      query
      language
  in
  let cols =
    [ `column Experiment.column_title; `column Experiment.column_public_title; `empty ]
  in
  let th_class = [ "w-7"; "w-5" ] in
  let row (experiment, pending) =
    let open Pool_message in
    let detail_btn = Page_admin_experiments.Partials.detail_button experiment in
    let session_modal_btn =
      let url =
        Format.asprintf
          "/admin/experiments/%s/contact-history/%s"
          Experiment.(Id.value experiment.id)
          Contact.(contact |> id |> Id.value)
        |> Sihl.Web.externalize_path
      in
      let attributes =
        Htmx.
          [ hx_get url
          ; hx_swap "outerHTML"
          ; hx_target ("#" ^ experiment_history_modal_id)
          ; a_class [ "primary"; "is-icon" ]
          ]
      in
      button ~a:attributes [ Icon.(to_html InformationOutline) ]
    in
    let buttons =
      div
        ~a:[ a_class [ "flexrow"; "flex-gap-sm"; "justify-end" ] ]
        [ session_modal_btn; detail_btn ]
    in
    let title =
      let text = txt Experiment.(Title.value experiment.title) in
      match pending with
      | false -> text
      | true ->
        div
          ~a:[ a_class [ "flexrow"; "flex-gap"; "align-center" ] ]
          [ span [ text ]
          ; span ~a:[ a_class [ "tag"; "primary"; "inline" ] ] [ txt "pending" ]
          ]
    in
    [ title, Some Field.Title
    ; txt Experiment.(PublicTitle.value experiment.public_title), Some Field.PublicTitle
    ; buttons, None
    ]
    |> CCList.map (fun (html, field) ->
      td ~a:(Component.Table.data_label_opt language field) [ html ])
    |> tr
  in
  DataTable.make
    ~break_mobile:true
    ~target_id:"experiment-history"
    ~th_class
    ~cols
    ~row
    data_table
    experiments
;;

let experiment_history_modal
      { Pool_context.csrf; language; _ }
      (experiment : Experiment.t)
      assignments
  =
  let open Pool_common in
  let title (_ : Language.t) = Experiment.(Title.value experiment.title) in
  let html =
    let thead =
      Pool_message.Field.[ Start; NoShow; Participated ]
      |> CCList.map CCFun.(Utils.field_to_string_capitalized language %> txt)
    in
    let thead = thead @ [ txt "" ] in
    let to_icon value =
      CCOption.map_or ~default:(txt "") CCFun.(value %> Component.Icon.bool_to_icon)
    in
    let rows =
      let open Assignment in
      let open Session in
      assignments
      |> CCList.map (fun (session, { id; no_show; participated; _ }) ->
        let start = start_end_with_duration_human session |> txt in
        let start =
          if CCOption.is_some session.follow_up_to
          then div ~a:[ a_class [ "inset"; "left" ] ] [ start ]
          else start
        in
        let forms =
          match session.closed_at with
          | Some _ -> txt ""
          | None ->
            let action suffix =
              Http_utils.Url.Admin.assignment_path
                experiment.Experiment.id
                session.id
                ~id
                ~suffix
                ()
              |> Sihl.Web.externalize_path
            in
            Pool_message.
              [ ( "cancel"
                , Pool_common.I18n.CancelAssignment
                , Control.(Cancel (Some Field.Assignment))
                , Icon.Close )
              ; ( "mark-as-deleted"
                , Pool_common.I18n.MarkAssignmentWithFollowUpsAsDeleted
                , Control.MarkAsDeleted
                , Icon.TrashOutline )
              ]
            |> CCList.map (fun (suffix, confirmable, control, icon) ->
              form
                ~a:
                  [ a_method `Post
                  ; a_action (action suffix)
                  ; a_user_data
                      "confirmable"
                      (Utils.confirmable_to_string language confirmable)
                  ]
                Component.Input.
                  [ csrf_element csrf ()
                  ; submit_icon
                      ~attributes:[ a_title (Utils.control_to_string language control) ]
                      ~classnames:[ "error" ]
                      icon
                  ])
            |> div ~a:[ a_class [ "flexrow"; "flex-gap-sm"; "justify-end" ] ]
        in
        [ start
        ; to_icon NoShow.value no_show
        ; to_icon Participated.value participated
        ; forms
        ])
    in
    Component.Table.horizontal_table ~align_last_end:true ~thead `Striped rows
  in
  Modal.create ~active:true language title experiment_history_modal_id html
;;

let detail
      ?admin_comment
      ~can_manage_duplicates
      (Pool_context.{ language; user; _ } as context)
      contact
      tags
      external_data_ids
      custom_fields
      past_experiments
  =
  let subtitle nav =
    h3
      ~a:[ a_class [ "heading-3" ] ]
      Pool_common.[ Utils.nav_link_to_string language nav |> txt ]
  in
  let buttons =
    let open Pool_common in
    let contact_path = path contact in
    let edit =
      Format.asprintf "%s/edit" contact_path
      |> Input.link_as_button
           ~icon:Icon.Create
           ~classnames:[ "small" ]
           ~control:(language, Pool_message.(Control.Edit (Some Field.Contact)))
    in
    let make_link path icon nav_link =
      a
        ~a:
          [ a_class [ "btn small primary is-text has-icon" ]
          ; a_href (Sihl.Web.externalize_path path)
          ]
        [ Icon.(to_html icon)
        ; txt Pool_common.(Utils.nav_link_to_string language nav_link)
        ]
    in
    let messages =
      make_link (Format.asprintf "%s/messages" contact_path) Icon.Mail I18n.MessageHistory
    in
    let duplicate =
      if can_manage_duplicates
      then (
        let path = Http_utils.Url.Admin.contact_duplicate_path (Contact.id contact) () in
        make_link path Icon.Person I18n.ManageDuplicates)
      else txt ""
    in
    div
      ~a:[ a_class [ "flexrow"; "flex-gap"; "flexcolumn-mobile"; "align-start" ] ]
      [ span ~a:[ a_class [ "flexrow"; "flex-gap-sm" ] ] [ duplicate; messages ]; edit ]
  in
  let past_experiments_html =
    let experiments, query = past_experiments in
    div
      [ div
          ~a:
            [ a_id experiment_history_modal_id
            ; a_class [ "modal"; "fullscreen-overlay" ]
            ]
          []
      ; h3
          ~a:[ a_class [ "has-gap" ] ]
          [ txt Pool_common.(Utils.text_to_string language I18n.ExperimentHistory) ]
      ; experiment_history context contact experiments query
      ]
  in
  let changelog_url =
    Http_utils.Url.Admin.contact_path ~suffix:"changelog" ~id:(Contact.id contact) ()
    |> Uri.of_string
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "stack-lg" ] ]
    [ div
        ~a:
          [ a_class [ "flexrow"; "wrap"; "flex-gap"; "justify-between"; "align-center" ] ]
        [ div [ heading_with_icons contact ]; buttons ]
    ; personal_detail ?admin_comment ~custom_fields ~tags user language contact
    ; div
        [ subtitle Pool_common.I18n.ExternalDataIds
        ; Component.Contacts.external_data_ids language external_data_ids
        ]
    ; div ~a:[ a_class [ "grid-col-2" ] ] [ assign_contact_form context contact ]
    ; past_experiments_html
    ; Component.Changelog.list context changelog_url None
    ]
;;

let tag_form (Pool_context.{ query_parameters; _ } as context) ?existing available contact
  =
  let action =
    Http_utils.externalize_path_with_params
      query_parameters
      (Format.asprintf
         "%s/%s/assign"
         (contact |> path)
         Pool_message.Field.(Tag |> human_url))
  in
  Component.Tag.add_tags_form ?existing context available action
;;

let edit
      ?(allowed_to_assign = false)
      ?(allowed_to_promote = false)
      (Pool_context.{ language; csrf; query_parameters; _ } as context)
      tenant_languages
      contact
      custom_fields
      tags
      available_tags
  =
  let open Page_contact_partials in
  let base_action = Htmx.admin_profile_hx_post (Contact.id contact) in
  let assign_tags =
    if allowed_to_assign
    then (
      let remove_action tag =
        Format.asprintf
          "%s/%s/%s/remove"
          base_action
          Pool_message.Field.(Tag |> human_url)
          Tags.(Id.value tag.id)
      in
      div
        [ h2
            ~a:[ a_class [ "heading-2"; "has-gap" ] ]
            Pool_common.[ Utils.nav_link_to_string language I18n.Tags |> txt ]
        ; div
            ~a:[ a_class [ "switcher-lg"; "flex-gap" ] ]
            [ tag_form context ~existing:tags available_tags contact
            ; Component.Tag.tag_form
                ~label:Pool_common.I18n.SelectedTags
                language
                (remove_action, csrf)
                tags
            ]
        ])
    else txt ""
  in
  let pause_form = pause_form csrf language query_parameters contact `Admin in
  let delete_form =
    match Pool_user.Disabled.value contact.Contact.disabled with
    | true -> None
    | false -> Some (mark_as_deleted_form csrf language query_parameters contact)
  in
  let promote_form =
    if allowed_to_promote
    then Some (promote_form csrf language query_parameters contact)
    else None
  in
  let verify_form = verify_form csrf language query_parameters contact in
  let status_forms =
    [ promote_form; Some verify_form; Some pause_form; delete_form ]
    |> CCList.filter_map CCFun.id
    |> status_form_table language
  in
  let form_context = `Admin in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ heading_with_icons contact
    ; div
        ~a:[ a_class [ "stack-lg" ] ]
        [ Page_contact_edit.personal_details_form
            csrf
            language
            query_parameters
            form_context
            tenant_languages
            contact
            custom_fields
        ; assign_tags
        ; status_forms
        ]
    ]
;;

let external_data_ids { Pool_context.language; _ } contact external_data_ids =
  let table = Component.Contacts.external_data_ids language external_data_ids in
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "stack" ] ]
    [ h1 [ txt (Contact.fullname contact) ]
    ; h2 [ txt Pool_common.(Utils.nav_link_to_string language I18n.ExternalDataIds) ]
    ; table
    ]
;;

let message_history_url contact =
  Uri.of_string
    (Format.asprintf "/admin/contacts/%s/messages" Contact.(contact |> id |> Id.value))
;;

let message_history ({ Pool_context.language; _ } as context) queue_table contact messages
  =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1"; "has-gap" ] ]
        [ txt
            Pool_common.(
              Utils.text_to_string
                language
                I18n.(MessageHistory (Contact.lastname_firstname contact)))
        ]
    ; Page_admin_queue.list context queue_table (message_history_url contact) messages
    ]
;;
