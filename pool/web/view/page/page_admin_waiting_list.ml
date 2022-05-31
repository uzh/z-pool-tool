open Tyxml.Html
open Component
module Message = Pool_common.Message

let detail
    (Waiting_list.{ id; contact; experiment; comment; _ } : Waiting_list.t)
    sessions
    experiment_id
    Pool_context.{ language; csrf; _ }
  =
  let waiting_list_detail =
    div
      ~a:[ a_class [ "stack" ] ]
      [ Page_admin_contact.personal_detail language contact
      ; form
          ~a:
            [ a_class [ "stack" ]
            ; a_method `Post
            ; a_action
                (let open Pool_common.Id in
                Sihl.Web.externalize_path
                  (Format.asprintf
                     "/admin/experiments/%s/waiting-list/%s"
                     (value experiment.Experiment.id)
                     (value id)))
            ]
          [ textarea_element
              language
              Pool_common.Message.Field.(show Comment)
              Pool_common.Message.Field.Comment
              (CCOption.map_or ~default:"" Waiting_list.Comment.value comment)
              ()
          ; submit_element
              language
              Pool_common.Message.(Save (Some Field.Comment))
              ()
          ]
      ]
  in
  let sessions =
    let thead =
      Pool_common.
        [ Utils.field_to_string language Message.Field.Date
        ; "Assign contact" (* TODO: Crate hints type?? *)
        ]
      |> CCList.map (fun text -> th [ txt text ])
      |> tr
      |> CCList.pure
      |> thead
    in
    CCList.map
      (fun (session : Session.t) ->
        tr
          [ td
              [ txt
                  Session.(
                    session.Session.start
                    |> Start.value
                    |> Pool_common.Utils.Time.formatted_date_time)
              ]
          ; td
              [ (match Session.is_fully_booked session with
                | false ->
                  input
                    ~a:
                      [ a_input_type `Radio
                      ; a_name Pool_common.Message.Field.(show Session)
                      ; a_value Session.(session.id |> Pool_common.Id.value)
                      ]
                    ()
                | true ->
                  span
                    [ txt
                        Pool_common.(
                          Utils.error_to_string
                            language
                            Message.SessionFullyBooked)
                    ])
              ]
          ])
      sessions
    |> table ~thead ~a:[ a_class [ "striped" ] ]
    |> fun content ->
    form
      ~a:
        [ a_method `Post
        ; a_action
            (Format.asprintf
               "/admin/experiments/%s/waiting-list/%s/assign"
               (experiment_id |> Pool_common.Id.value)
               (id |> Pool_common.Id.value)
            |> Sihl.Web.externalize_path)
        ]
      [ Component.csrf_element csrf ()
      ; content
      ; div
          ~a:[ a_class [ "gap" ] ]
          [ Component.submit_element
              language
              Pool_common.Message.(Assign (Some Field.Contact))
              ()
          ]
      ]
  in
  let html =
    div ~a:[ a_class [ "stack-lg" ] ] [ waiting_list_detail; sessions ]
  in
  Page_admin_experiments.experiment_layout
    language
    Pool_common.I18n.ExperimentWaitingListTitle
    experiment
    html
;;
