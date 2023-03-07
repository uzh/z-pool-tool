open Tyxml.Html
open Component.Input
module Message = Pool_common.Message

let detail
  (Waiting_list.{ id; contact; experiment; comment; _ } : Waiting_list.t)
  sessions
  experiment_id
  Pool_context.{ language; csrf; _ }
  flash_fetcher
  =
  let open Pool_common in
  let waiting_list_detail =
    div
      ~a:[ a_class [ "stack" ] ]
      [ Page_admin_contact.personal_detail language contact
      ; form
          ~a:
            [ a_class [ "stack" ]
            ; a_method `Post
            ; a_action
                (Sihl.Web.externalize_path
                   (Format.asprintf
                      "/admin/experiments/%s/waiting-list/%s"
                      (Experiment.Id.value experiment.Experiment.id)
                      (Id.value id)))
            ]
          [ csrf_element csrf ()
          ; textarea_element
              language
              Message.Field.Comment
              ~value:
                (CCOption.map_or ~default:"" Waiting_list.Comment.value comment)
              ~flash_fetcher
          ; div
              ~a:[ a_class [ "flexrow" ] ]
              [ submit_element
                  ~classnames:[ "push" ]
                  language
                  Message.(Save (Some Field.Comment))
                  ()
              ]
          ]
      ]
  in
  let sessions =
    let content =
      if CCList.is_empty sessions
      then
        div
          [ txt
              (Utils.text_to_string
                 language
                 (I18n.EmtpyList Message.Field.Sessions))
          ]
      else (
        let thead =
          Message.
            [ Field.Date |> Component.Table.field_to_txt language; txt "" ]
        in
        let to_row (session : Session.t) =
          let attrs =
            if CCOption.is_some session.Session.follow_up_to
            then [ a_class [ "inset"; "left" ] ]
            else []
          in
          [ div
              ~a:attrs
              [ txt
                  Session.(
                    session.Session.start
                    |> Start.value
                    |> Utils.Time.formatted_date_time)
              ]
          ; (match
               Session.is_fully_booked session, session.Session.follow_up_to
             with
             | false, None ->
               input
                 ~a:
                   [ a_input_type `Radio
                   ; a_name Message.Field.(show Session)
                   ; a_value Session.(session.id |> Id.value)
                   ]
                 ()
             | false, Some _ ->
               span
                 [ txt
                     (Utils.error_to_string
                        language
                        Message.SessionRegistrationViaParent)
                 ]
             | true, _ ->
               span
                 [ txt
                     (Utils.error_to_string language Message.SessionFullyBooked)
                 ])
          ]
        in
        let to_rows (session, follow_ups) =
          session :: follow_ups |> CCList.flat_map to_row
        in
        Component.Table.horizontal_table
          ~align_last_end:true
          `Striped
          ~thead
          (sessions |> CCList.map to_rows)
        |> fun content ->
        match experiment |> Experiment.registration_disabled_value with
        | true ->
          div
            [ content
            ; div
                ~a:[ a_class [ "gap" ] ]
                [ div
                    ~a:[ a_class [ "flexrow" ] ]
                    [ submit_element
                        language
                        ~classnames:[ "disabled"; "push" ]
                        (Message.Assign (Some Field.Contact))
                        ()
                    ]
                ; p
                    ~a:[ a_class [ "help" ] ]
                    [ txt
                        (Utils.error_to_string
                           language
                           Message.RegistrationDisabled)
                    ]
                ]
            ]
        | false ->
          form
            ~a:
              [ a_method `Post
              ; a_action
                  (Format.asprintf
                     "/admin/experiments/%s/waiting-list/%s/assign"
                     (experiment_id |> Experiment.Id.value)
                     (id |> Id.value)
                   |> Sihl.Web.externalize_path)
              ]
            [ csrf_element csrf ()
            ; content
            ; div
                ~a:[ a_class [ "gap"; "flexrow" ] ]
                [ submit_element
                    ~classnames:[ "push" ]
                    language
                    (Message.Assign (Some Field.Contact))
                    ()
                ]
            ])
    in
    div
      [ h2
          ~a:[ a_class [ "heading-2" ] ]
          [ txt (Utils.nav_link_to_string language I18n.Sessions) ]
      ; p
          [ txt
              (I18n.AssignContactFromWaitingList
               |> Utils.hint_to_string language)
          ]
      ; content
      ]
  in
  let html =
    div ~a:[ a_class [ "stack-lg" ] ] [ waiting_list_detail; sessions ]
  in
  Page_admin_experiments.experiment_layout
    language
    (Page_admin_experiments.NavLink I18n.WaitingList)
    experiment
    html
;;
