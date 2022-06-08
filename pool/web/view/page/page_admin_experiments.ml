open Tyxml.Html
open Component
module Message = Pool_common.Message

(* TODO [timhub]: create global nav component, when MR is merged *)
let subnav language active id =
  let open Pool_common in
  I18n.
    [ Overview, "/"
    ; Invitations, "/invitations"
    ; WaitingList, "/waiting-list"
    ; Sessions, "/sessions"
    ; Assignments, "/assignments"
    ]
  |> CCList.map (fun (label, url) ->
         let is_active =
           active
           |> CCOption.map_or ~default:false (fun active ->
                  I18n.equal_nav_link active label)
         in
         let classnames = [] in
         let link_label = txt (Utils.nav_link_to_string language label) in
         if is_active
         then
           span ~a:[ a_class ([ "color-primary" ] @ classnames) ] [ link_label ]
         else
           a
             ~a:
               [ a_href
                   (Sihl.Web.externalize_path
                      (Format.asprintf
                         "/admin/experiments/%s/%s"
                         (Id.value id)
                         url))
               ; a_class classnames
               ]
             [ link_label ])
  |> nav ~a:[ a_class [ "sub-nav"; "flexrow"; "flex-gap" ] ]
;;

type title =
  | Control of Pool_common.Message.control
  | NavLink of Pool_common.I18n.nav_link
  | I18n of Pool_common.I18n.t

let title_to_string language text =
  let open Pool_common.Utils in
  match text with
  | Control text -> control_to_string language text
  | NavLink text -> nav_link_to_string language text
  | I18n text -> text_to_string language text
;;

let experiment_layout language title experiment_id ?active html =
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "measure" ] ]
    [ subnav language active experiment_id
    ; h1 ~a:[ a_class [ "heading-1" ] ] [ txt (title_to_string language title) ]
    ; html
    ]
;;

let index experiment_list Pool_context.{ language; _ } =
  let experiment_item (experiment : Experiment.t) =
    let open Experiment in
    div
      ~a:[ a_class [ "flexrow"; "space-between"; "inset-sm" ] ]
      [ span [ txt (Title.value experiment.title) ]
      ; a
          ~a:
            [ a_href
                (Sihl.Web.externalize_path
                   (Format.asprintf
                      "/admin/experiments/%s"
                      (experiment.id |> Pool_common.Id.value)))
            ]
          [ txt Pool_common.(Message.More |> Utils.control_to_string language) ]
      ]
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "measure" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt
            Pool_common.(Utils.text_to_string language I18n.ExperimentListTitle)
        ]
    ; div
        ~a:[ a_class [ "stack" ] ]
        [ div
            ~a:[ a_class [ "striped" ] ]
            (CCList.map experiment_item experiment_list)
        ; p
            [ a
                ~a:
                  [ a_href
                      (Sihl.Web.externalize_path "/admin/experiments/create")
                  ]
                [ txt
                    Pool_common.(
                      Message.(Add (Some Field.Experiment))
                      |> Utils.control_to_string language)
                ]
            ]
        ]
    ]
;;

let experiment_form ?experiment Pool_context.{ language; csrf; _ } =
  let open Experiment in
  let action =
    match experiment with
    | None -> "/admin/experiments"
    | Some experiment ->
      Format.asprintf
        "/admin/experiments/%s"
        (experiment.id |> Pool_common.Id.value)
  in
  let value = CCFun.flip (CCOption.map_or ~default:"") experiment in
  form
    ~a:
      [ a_method `Post
      ; a_action (Sihl.Web.externalize_path action)
      ; a_class [ "stack" ]
      ]
    [ Component.csrf_element csrf ()
    ; input_element
        language
        `Text
        Pool_common.Message.Field.Title
        (value title_value)
    ; input_element
        language
        `Text
        Pool_common.Message.Field.Description
        (value description_value)
    ; checkbox_element
        language
        `Checkbox
        Pool_common.Message.Field.WaitingListDisabled
        (experiment
        |> CCOption.map_or ~default:false waiting_list_disabled_value)
    ; checkbox_element
        language
        `Checkbox
        Pool_common.Message.Field.DirectRegistrationDisabled
        (experiment
        |> CCOption.map_or ~default:false direct_registration_disabled_value)
    ; checkbox_element
        language
        `Checkbox
        Pool_common.Message.Field.RegistrationDisabled
        (experiment
        |> CCOption.map_or ~default:false registration_disabled_value)
    ; submit_element
        language
        Message.(
          let field = Some Field.Experiment in
          match experiment with
          | None -> Create field
          | Some _ -> Update field)
        ~submit_type:`Success
        ()
    ]
;;

let create (Pool_context.{ language; _ } as context) =
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "measure"; "stack" ] ]
    [ h1
        [ txt
            Pool_common.(
              Utils.control_to_string
                language
                Message.(Create (Some Field.Experiment)))
        ]
    ; experiment_form context
    ]
;;

let edit experiment (Pool_context.{ language; _ } as context) =
  let html = experiment_form ~experiment context in
  experiment_layout
    language
    (Control Pool_common.Message.(Edit (Some Field.Experiment)))
    experiment.Experiment.id
    html
;;

let detail experiment session_count Pool_context.{ language; _ } =
  let delete_form =
    match session_count > 0 with
    | true ->
      div
        [ submit_element
            language
            Message.(Delete (Some Field.Experiment))
            ~submit_type:`Disabled
            ()
        ; p
            [ small
                [ txt
                    Pool_common.(
                      Message.ExperimentSessionCountNotZero
                      |> Utils.error_to_string language)
                ]
            ]
        ]
    | false ->
      Tyxml.Html.form
        ~a:
          [ a_method `Post
          ; a_action
              (Sihl.Web.externalize_path
                 (Format.asprintf
                    "/admin/experiments/%s/delete"
                    (experiment.Experiment.id |> Pool_common.Id.value)))
          ]
        [ submit_element
            language
            Message.(Delete (Some Field.Experiment))
            ~submit_type:`Error
            ~has_icon:`TrashOutline
            ()
        ]
  in
  let field_to_string = Pool_common.Utils.field_to_string language in
  let bool_to_string = Pool_common.Utils.bool_to_string language in
  let open Experiment in
  let html =
    let boolean_fields =
      let open Experiment in
      Message.Field.
        [ WaitingListDisabled, waiting_list_disabled_value
        ; DirectRegistrationDisabled, direct_registration_disabled_value
        ; RegistrationDisabled, registration_disabled_value
        ]
    in
    div
      [ p [ txt (experiment.description |> Description.value) ]
      ; table
          (boolean_fields
          |> CCList.map (fun (label, fnc) ->
                 tr
                   [ td [ txt (field_to_string label) ]
                   ; td [ txt (fnc experiment |> bool_to_string) ]
                   ]))
      ; p
          [ a
              ~a:
                [ a_href
                    (Sihl.Web.externalize_path
                       (Format.asprintf
                          "/admin/experiments/%s/edit"
                          (experiment.id |> Pool_common.Id.value)))
                ]
              [ txt
                  Pool_common.(
                    Message.(Edit (Some Field.Experiment))
                    |> Utils.control_to_string language)
              ]
          ]
      ; delete_form
      ]
  in
  experiment_layout
    language
    (NavLink Pool_common.I18n.Experiments)
    experiment.Experiment.id
    ~active:Pool_common.I18n.Overview
    html
;;

let invitations
    invitations
    experiment
    filtered_contacts
    (Pool_context.{ language; _ } as context)
  =
  let html =
    div
      ~a:[ a_class [ "stack-lg" ] ]
      [ Page_admin_invitations.Partials.list context experiment invitations
      ; Page_admin_invitations.Partials.send_invitation
          context
          experiment
          filtered_contacts
      ]
  in
  experiment_layout
    language
    (NavLink Pool_common.I18n.Invitations)
    experiment.Experiment.id
    ~active:Pool_common.I18n.Invitations
    html
;;

let waiting_list waiting_list experiment Pool_context.{ language; _ } =
  let open Waiting_list.ExperimentList in
  let waiting_list_entries () =
    let thead =
      Table.head
        language
        Pool_common.Message.Field.
          [ Some Name; Some Email; Some CreatedAt; Some Comment; None ]
    in
    CCList.map
      (fun entry ->
        tr
          [ td [ txt (Contact.Preview.fullname entry.contact) ]
          ; td
              [ txt
                  (Contact.Preview.email_address entry.contact
                  |> Pool_user.EmailAddress.value)
              ]
          ; td
              [ txt
                  Pool_common.(
                    entry.created_at
                    |> CreatedAt.value
                    |> Utils.Time.formatted_date_time)
              ]
          ; td
              [ entry.comment
                |> CCOption.map_or ~default:"" Waiting_list.Comment.value
                |> txt
              ]
          ; td
              [ a
                  ~a:
                    [ a_href
                        (Sihl.Web.externalize_path
                           (Format.asprintf
                              "/admin/experiments/%s/waiting-list/%s"
                              (waiting_list.experiment.Experiment.id
                              |> Pool_common.Id.value)
                              (entry.id |> Pool_common.Id.value)))
                    ]
                  [ txt
                      Pool_common.(
                        Message.More |> Utils.control_to_string language)
                  ]
              ]
          ])
      waiting_list.waiting_list_entries
    |> table ~thead ~a:[ a_class [ "striped" ] ]
  in
  let content =
    match waiting_list.experiment.Experiment.waiting_list_disabled with
    | false -> waiting_list_entries ()
    | true ->
      p
        [ txt
            Pool_common.(
              Utils.text_to_string language I18n.WaitingListIsDisabled)
        ]
  in
  experiment_layout
    language
    (NavLink Pool_common.I18n.WaitingList)
    experiment
    ~active:Pool_common.I18n.WaitingList
    content
;;
