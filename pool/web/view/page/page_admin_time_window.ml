open Tyxml.Html
open Component
open Input
module Message = Pool_common.Message

let session_path = HttpUtils.Url.Admin.session_path

let time_window_form
  csrf
  language
  ?time_window
  (experiment : Experiment.t)
  ~flash_fetcher
  =
  let open CCFun in
  let open Session in
  let open Time_window in
  let open Pool_common in
  let value = CCFun.flip (CCOption.map_or ~default:"") time_window in
  let action, submit =
    let path ?id () =
      session_path ?id experiment.Experiment.id |> Sihl.Web.externalize_path
    in
    match time_window with
    | None -> path (), Message.(Create (Some Field.Session))
    | Some session ->
      path ~id:session.Time_window.id (), Message.(Update (Some Field.Session))
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
    date_input ~read_only Message.Field.Start value
  in
  let end_field =
    let value = time_window |> CCOption.map Time_window.ends_at in
    let min_value =
      time_window
      |> CCOption.map (fun ({ start; _ } : Time_window.t) -> Start.value start)
    in
    date_input ?min_value Message.Field.End value
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
            Message.Field.InternalDescription
            ~value:
              (value (fun s ->
                 s.internal_description
                 |> CCOption.map_or ~default:"" InternalDescription.value))
            ~flash_fetcher
        ; textarea_element
            language
            Message.Field.PublicDescription
            ~value:
              (value (fun s ->
                 s.public_description
                 |> CCOption.map_or ~default:"" PublicDescription.value))
            ~flash_fetcher
        ; input_element
            language
            `Number
            Message.Field.MaxParticipants
            ?value:
              CCOption.(
                bind time_window (fun s ->
                  map
                    (ParticipantAmount.value %> CCInt.to_string)
                    s.max_participants))
            ~flash_fetcher
        ]
    ; div
        ~a:[ a_class [ "flexrow" ] ]
        [ submit_element ~classnames:[ "push" ] language submit () ]
    ]
;;

let new_form
  ({ Pool_context.language; csrf; _ } as context)
  experiment
  flash_fetcher
  =
  time_window_form csrf language experiment ~flash_fetcher
  |> CCList.return
  |> Layout.Experiment.(
       create
         context
         (Control Message.(Create (Some Field.TimeWindow)))
         experiment)
;;

let edit
  ({ Pool_context.language; csrf; _ } as context)
  experiment
  time_window
  tags
  flash_fetcher
  =
  let tags_html =
    Page_admin_session.tags_subform
      context
      experiment
      (`TimeWindow time_window)
      tags
  in
  let form =
    time_window_form csrf language experiment ~time_window ~flash_fetcher
  in
  div ~a:[ a_class [ "stack-lg" ] ] [ form; tags_html ]
  |> CCList.return
  |> Layout.Experiment.(
       create
         context
         (Control Message.(Edit (Some Field.TimeWindow)))
         experiment)
;;

let detail
  ?access_contact_profiles
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
  let open Pool_common in
  let open Time_window in
  let session_path = session_path ~id:time_window.id experiment.Experiment.id in
  let edit_button =
    link_as_button
      ~icon:Icon.Create
      ~classnames:[ "small" ]
      ~control:(language, Message.(Edit (Some Field.Session)))
      (Format.asprintf "%s/edit" session_path)
  in
  let overview =
    let open Message in
    let open Session in
    let int_to_txt i = i |> CCInt.to_string |> txt in
    let amount amt = amt |> ParticipantAmount.value |> string_of_int in
    let date date = Pool_common.Utils.Time.formatted_date_time date in
    [ Field.Start, time_window.start |> Start.value |> date |> txt
    ; Field.End, ends_at time_window |> date |> txt
    ; ( Field.InternalDescription
      , CCOption.map_or
          ~default:""
          InternalDescription.value
          time_window.internal_description
        |> Http_utils.add_line_breaks )
    ; ( Field.PublicDescription
      , CCOption.map_or
          ~default:""
          PublicDescription.value
          time_window.public_description
        |> Http_utils.add_line_breaks )
    ; ( Field.MaxParticipants
      , time_window.max_participants
        |> CCOption.map_or ~default:"" amount
        |> txt )
    ; ( Field.ParticipantCount
      , time_window.participant_count |> ParticipantCount.value |> int_to_txt )
    ; Field.NoShow, time_window.no_show_count |> NoShowCount.value |> int_to_txt
    ; ( Field.MaxParticipants
      , time_window.max_participants
        |> CCOption.map_or ~default:"" amount
        |> txt )
    ; ( Field.ClosedAt
      , time_window.closed_at |> CCOption.map_or ~default:"" date |> txt )
    ; ( Field.CanceledAt
      , time_window.canceled_at |> CCOption.map_or ~default:"" date |> txt )
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
      experiment
      (`TimeWindow time_window)
      text_messages_enabled
      assignments
  in
  let tags_html =
    div
      [ h2
          ~a:[ a_class [ "heading-2" ] ]
          Pool_common.[ Utils.nav_link_to_string language I18n.Tags |> txt ]
      ; Component.Tag.tag_list language participation_tags
      ]
  in
  div ~a:[ a_class [ "stack-lg" ] ] [ overview; tags_html; assignments_html ]
  |> CCList.return
  |> Layout.Experiment.(
       create
         ~buttons:edit_button
         context
         (I18n (HttpUtils.Session.timewindow_title time_window))
         experiment)
;;
