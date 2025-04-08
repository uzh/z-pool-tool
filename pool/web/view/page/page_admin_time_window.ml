open Tyxml.Html
open Component
open Input
open Pool_message
open Control

let session_path = HttpUtils.Url.Admin.session_path

module Partials = struct
  let table_legend language =
    let open Pool_common in
    let to_string = Utils.text_to_string language in
    let open Component.Table in
    let closed = I18n.(to_string Closed, legend_color_item "bg-green-lighter") in
    [ closed ] |> table_legend
  ;;

  let row_classnames time_window =
    let check opt classname = if opt then Some classname else None in
    [ check
        (Time_window.ends_at time_window |> Ptime.is_earlier ~than:(Ptime_clock.now ()))
        "bg-green-lighter"
    ]
    |> CCList.filter_map CCFun.id
  ;;

  let row_attrs time_window =
    let classnames = row_classnames time_window in
    [ a_class classnames ]
  ;;

  let detail_button language experiment_id time_window_id =
    HttpUtils.Url.Admin.session_path experiment_id ~id:time_window_id
    |> link_as_button ~is_text:true ~icon:Icon.Eye ~control:(language, Details)
  ;;

  let button_dropdown
        { Pool_context.language; _ }
        experiment_id
        time_window
        ~can_access_session_assistants
    =
    [ detail_button language experiment_id time_window.Time_window.id |> CCOption.return
    ; Page_admin_session.Partials.assistants_button
        language
        experiment_id
        time_window.Time_window.id
        can_access_session_assistants
    ]
    |> CCList.filter_map CCFun.id
    |> fun buttons ->
    div
      ~a:[ a_class [ "flexrow" ] ]
      [ Component.ButtonGroup.dropdown ~classnames:[ "push" ] buttons ]
  ;;
end

let time_window_form csrf language ?time_window (experiment : Experiment.t) ~flash_fetcher
  =
  let open CCFun in
  let open Session in
  let open Time_window in
  let value = CCFun.flip (CCOption.map_or ~default:"") time_window in
  let action, submit =
    let path ?id () =
      session_path ?id experiment.Experiment.id |> Sihl.Web.externalize_path
    in
    match time_window with
    | None -> path (), Create (Some Field.Session)
    | Some session -> path ~id:session.Time_window.id (), Update (Some Field.Session)
  in
  let is_past = function
    | None -> false
    | Some value -> Ptime.is_earlier value ~than:(Ptime_clock.now ())
  in
  let date_input ?min_value ?read_only field value =
    date_time_picker_element
      language
      ~required:true
      ~flash_fetcher
      ?value
      ?min_value
      ~disable_past:(CCOption.is_none time_window)
      ~warn_past:true
      ?read_only
      field
  in
  let start_field =
    let value =
      time_window
      |> CCOption.map (fun ({ start; _ } : Time_window.t) -> Start.value start)
    in
    let read_only = is_past value in
    date_input ~read_only Field.Start value
  in
  let end_field =
    let value = time_window |> CCOption.map Time_window.ends_at in
    let min_value =
      time_window
      |> CCOption.map (fun ({ start; _ } : Time_window.t) -> Start.value start)
    in
    date_input ?min_value Field.End value
  in
  form
    ~a:
      [ a_class [ "stack" ]
      ; a_method `Post
      ; a_action (action |> Sihl.Web.externalize_path)
      ; a_user_data "detect-unsaved-changes" ""
      ]
    [ csrf_element csrf ()
    ; div
        ~a:[ a_class [ "grid-col-2" ] ]
        [ start_field
        ; end_field
        ; textarea_element
            language
            Field.InternalDescription
            ~value:
              (value (fun s ->
                 s.internal_description
                 |> CCOption.map_or ~default:"" InternalDescription.value))
            ~flash_fetcher
        ; textarea_element
            language
            Field.PublicDescription
            ~value:
              (value (fun s ->
                 s.public_description
                 |> CCOption.map_or ~default:"" PublicDescription.value))
            ~flash_fetcher
        ; input_element
            language
            `Number
            Field.MaxParticipants
            ?value:
              CCOption.(
                bind time_window (fun s ->
                  map (ParticipantAmount.value %> CCInt.to_string) s.max_participants))
            ~flash_fetcher
        ]
    ; div
        ~a:[ a_class [ "flexrow" ] ]
        [ submit_element ~classnames:[ "push" ] language submit () ]
    ]
;;

let new_form ({ Pool_context.language; csrf; _ } as context) experiment flash_fetcher =
  time_window_form csrf language experiment ~flash_fetcher
  |> CCList.return
  |> Layout.Experiment.(
       create context (Control (Create (Some Field.TimeWindow))) experiment)
;;

let edit
      ({ Pool_context.language; csrf; _ } as context)
      experiment
      time_window
      tags
      flash_fetcher
  =
  let tags_html =
    Page_admin_session.tags_subform context experiment (`TimeWindow time_window) tags
  in
  let form = time_window_form csrf language experiment ~time_window ~flash_fetcher in
  div ~a:[ a_class [ "stack-lg" ] ] [ form; tags_html ]
  |> CCList.return
  |> Layout.Experiment.(
       create context (Control (Edit (Some Field.TimeWindow))) experiment)
;;

let data_table
      ({ Pool_context.language; _ } as context)
      ?(can_access_session_assistants = false)
      experiment
      (time_windows, query)
  =
  let open Session in
  let time_windows_path ?id ?suffix () =
    session_path ?id ?suffix experiment.Experiment.id
  in
  let data_table =
    let url = time_windows_path () |> Uri.of_string in
    Component.DataTable.create_meta url query language
  in
  let cols =
    let create_session : [ | Html_types.flow5 ] elt =
      link_as_button
        ~style:`Success
        ~icon:Icon.Add
        ~classnames:[ "small"; "nowrap" ]
        ~control:(language, Add (Some Field.TimeWindow))
        (time_windows_path ~suffix:"create" ())
    in
    [ `column column_date
    ; `column column_no_assignments
    ; `column column_noshow_count
    ; `column column_participation_count
    ; `custom (txt "Max")
    ; `mobile create_session
    ]
  in
  let th_class = [ "w-4"; "w-2"; "w-2"; "w-2"; "w-1"; "w-1" ] in
  let row
        ({ Time_window.assignment_count
         ; no_show_count
         ; participant_count
         ; max_participants
         ; _
         } as time_window :
          Time_window.t)
    =
    let open Time_window in
    let int_to_txt i = CCInt.to_string i |> txt in
    let row_attrs = Partials.row_attrs time_window in
    let is_terminated =
      ends_at time_window |> Ptime.is_earlier ~than:(Ptime_clock.now ())
    in
    let no_show_count =
      if is_terminated then no_show_count |> NoShowCount.value |> int_to_txt else txt ""
    in
    [ start_end_with_duration_human time_window |> txt
    ; assignment_count |> AssignmentCount.value |> int_to_txt
    ; no_show_count
    ; participant_count |> ParticipantCount.value |> int_to_txt
    ; max_participants
      |> CCOption.map_or ~default:(txt "") CCFun.(ParticipantAmount.value %> int_to_txt)
    ; Partials.button_dropdown
        context
        experiment.Experiment.id
        time_window
        ~can_access_session_assistants
    ]
    |> CCList.map CCFun.(CCList.return %> td)
    |> tr ~a:row_attrs
  in
  DataTable.make
    ~align_last_end:true
    ~break_mobile:true
    ~target_id:"timewindow-list"
    ~th_class
    ~cols
    ~row
    data_table
    time_windows
;;

let index
      ({ Pool_context.language; _ } as context)
      ?(can_access_session_assistants = false)
      experiment
      sessions
  =
  let open Pool_common in
  div
    ~a:[ a_class [ "stack" ] ]
    [ Partials.table_legend language
    ; data_table ~can_access_session_assistants context experiment sessions
    ]
  |> CCList.return
  |> Layout.Experiment.(
       create
         ~active_navigation:"sessions"
         ~hint:I18n.ExperimentSessions
         context
         (NavLink I18n.Sessions)
         experiment)
;;

let detail
      ?access_contact_profiles
      ?(can_access_session_assistants = false)
      ?send_direct_message
      ?view_contact_name
      ?view_contact_info
      ~text_messages_enabled
      (Pool_context.{ language; _ } as context)
      experiment
      (time_window : Time_window.t)
      participation_tags
      assignments
  =
  let open Time_window in
  let buttons =
    Page_admin_session.Partials.title_buttons
      language
      time_window.id
      time_window.experiment.Experiment.id
      can_access_session_assistants
  in
  let overview =
    let open Session in
    let int_to_txt i = i |> CCInt.to_string |> txt in
    let amount amt = amt |> ParticipantAmount.value |> string_of_int in
    let date date = Pool_model.Time.formatted_date_time date in
    [ Field.Start, time_window.start |> Start.value |> date |> txt
    ; Field.End, ends_at time_window |> date |> txt
    ; ( Field.InternalDescription
      , CCOption.map_or
          ~default:""
          InternalDescription.value
          time_window.internal_description
        |> Http_utils.add_line_breaks )
    ; ( Field.PublicDescription
      , CCOption.map_or ~default:"" PublicDescription.value time_window.public_description
        |> Http_utils.add_line_breaks )
    ; ( Field.MaxParticipants
      , time_window.max_participants |> CCOption.map_or ~default:"" amount |> txt )
    ; ( Field.ParticipantCount
      , time_window.participant_count |> ParticipantCount.value |> int_to_txt )
    ; Field.NoShow, time_window.no_show_count |> NoShowCount.value |> int_to_txt
    ; ( Field.MaxParticipants
      , time_window.max_participants |> CCOption.map_or ~default:"" amount |> txt )
    ]
    |> Table.vertical_table `Striped language ~align_top:true
  in
  let assignments_html =
    Page_admin_assignments.data_table
      ?access_contact_profiles
      ?send_direct_message
      ?view_contact_name
      ?view_contact_info
      context
      (`TimeWindow time_window)
      text_messages_enabled
      assignments
  in
  let tags_html =
    div
      [ h2
          ~a:[ a_class [ "heading-2"; "has-gap" ] ]
          Pool_common.[ Utils.nav_link_to_string language I18n.Tags |> txt ]
      ; Component.Tag.tag_list language participation_tags
      ]
  in
  div ~a:[ a_class [ "stack-lg" ] ] [ overview; tags_html; assignments_html ]
  |> CCList.return
  |> Layout.Experiment.(
       create
         ~buttons
         context
         (I18n (HttpUtils.Session.timewindow_title time_window))
         experiment)
;;
