open Tyxml.Html
open Component.Input
module Message = Pool_common.Message

let detail
  (Waiting_list.{ id; contact; experiment; admin_comment; _ } : Waiting_list.t)
  sessions
  experiment_id
  (Pool_context.{ language; csrf; _ } as context)
  flash_fetcher
  chronological
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
              Message.Field.AdminComment
              ~value:
                (CCOption.map_or
                   ~default:""
                   Waiting_list.AdminComment.value
                   admin_comment)
              ~flash_fetcher
          ; div
              ~a:[ a_class [ "flexrow" ] ]
              [ submit_element
                  ~classnames:[ "push" ]
                  language
                  Message.(Save (Some Field.AdminComment))
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
      else
        Page_admin_session.session_list
          `WaitingList
          context
          experiment_id
          sessions
          chronological
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
            ]
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
  div ~a:[ a_class [ "stack-lg" ] ] [ waiting_list_detail; sessions ]
  |> CCList.return
  |> Layout.Experiment.(create context (NavLink I18n.WaitingList) experiment)
;;
