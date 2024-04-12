open Tyxml.Html
open Component
open Input
module Message = Pool_common.Message

let time_window_form
  csrf
  language
  ?(session : Time_window.t option)
  (experiment : Experiment.t)
  ~flash_fetcher
  =
  let open CCFun in
  let open Session in
  let open Time_window in
  let open Pool_common in
  let value = CCFun.flip (CCOption.map_or ~default:"") session in
  let action, submit =
    let path ?id () =
      HttpUtils.Url.Admin.session_path ?id experiment.Experiment.id
      |> Sihl.Web.externalize_path
    in
    match session with
    | None -> path (), Message.(Create (Some Field.Session))
    | Some session ->
      path ~id:session.Time_window.id (), Message.(Update (Some Field.Session))
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
        [ date_time_picker_element
            language
            ~required:true
            ~flash_fetcher
            ?value:
              (CCOption.map (fun (s : t) -> s.start |> Start.value) session)
            ~disable_past:true
            Message.Field.Start
        ; date_time_picker_element
            language
            ~required:true
            ~flash_fetcher
            ?value:(CCOption.map ends_at session)
            ~disable_past:true
            Message.Field.End
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
                bind session (fun s ->
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
       create context (Control Message.(Create (Some Field.Session))) experiment)
;;
