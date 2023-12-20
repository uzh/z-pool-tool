open Tyxml.Html
open Pool_common
open Component.Input
module Table = Component.Table
module Message = Pool_common.Message

let detail
  (Waiting_list.{ id; contact; experiment; admin_comment; _ } : Waiting_list.t)
  sessions
  experiment_id
  (Pool_context.{ language; csrf; user; _ } as context)
  flash_fetcher
  chronological
  =
  let waiting_list_detail =
    div
      ~a:[ a_class [ "stack" ] ]
      [ Page_admin_contact.personal_detail user language contact
      ; form
          ~a:
            [ a_class [ "stack" ]
            ; a_method `Post
            ; a_action
                (Sihl.Web.externalize_path
                   (Format.asprintf
                      "/admin/experiments/%s/waiting-list/%s"
                      (Experiment.Id.value experiment.Experiment.id)
                      (Waiting_list.Id.value id)))
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
                     (id |> Waiting_list.Id.value)
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

let index
  experiment
  (waiting_list, query)
  ({ Pool_context.language; _ } as context)
  =
  let waiting_list_table waiting_list_entries =
    let thead =
      (Field.[ Name; Email; CellPhone; SignedUpAt; AdminComment ]
       |> Component.Table.fields_to_txt language)
      @ [ txt "" ]
    in
    let rows =
      let open CCOption in
      CCList.map
        Contact.(
          fun ({ Waiting_list.id; contact; admin_comment; created_at; _ } :
                Waiting_list.t) ->
            [ txt (fullname contact)
            ; txt (email_address contact |> Pool_user.EmailAddress.value)
            ; txt
                (contact.cell_phone
                 |> map_or ~default:"" Pool_user.CellPhone.value)
            ; txt
                (created_at |> CreatedAt.value |> Utils.Time.formatted_date_time)
            ; admin_comment
              |> map_or ~default:"" Waiting_list.AdminComment.value
              |> HttpUtils.first_n_characters
              |> HttpUtils.add_line_breaks
            ; Format.asprintf
                "/admin/experiments/%s/waiting-list/%s"
                (experiment.Experiment.id |> Experiment.Id.value)
                (id |> Waiting_list.Id.value)
              |> edit_link
            ])
        waiting_list_entries
    in
    Table.horizontal_table
      `Striped
      ~align_top:true
      ~align_last_end:true
      ~thead
      rows
  in
  Component.List.create
    language
    waiting_list_table
    Waiting_list.searchable_by
    (waiting_list, query)
  |> CCList.return
  |> Layout.Experiment.(
       create
         ~active_navigation:I18n.WaitingList
         ~hint:I18n.ExperimentWaitingList
         context
         (NavLink I18n.WaitingList)
         experiment)
;;
