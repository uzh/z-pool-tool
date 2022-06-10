open Tyxml.Html
open Component
module Message = Pool_common.Message

let mailing_title (s : Mailing.t) =
  Pool_common.I18n.MailingDetailTitle
    (s.Mailing.start_at |> Mailing.StartAt.value)
;;

let index Pool_context.{ language; csrf; _ } experiment mailings =
  let experiment_id = experiment.Experiment.id in
  let mailing_row (mailing : Mailing.t) =
    let open Mailing in
    tr
      [ td [ mailing.start_at |> StartAt.to_human |> txt ]
      ; td [ mailing.end_at |> EndAt.to_human |> txt ]
      ; td [ mailing.rate |> Rate.value |> CCInt.to_string |> txt ]
      ; td
          [ a
              ~a:
                [ a_href
                    (Format.asprintf
                       "/admin/experiments/%s/mailings/%s"
                       (Pool_common.Id.value experiment_id)
                       (Id.value mailing.id)
                    |> Sihl.Web.externalize_path)
                ]
              [ txt
                  Pool_common.(Utils.control_to_string language Message.(More))
              ]
          ]
      ; td
          [ form
              ~a:
                [ a_method `Post
                ; a_action
                    (Format.asprintf
                       "/admin/experiments/%s/mailings/%s/stop"
                       (Pool_common.Id.value experiment_id)
                       (Id.value mailing.id)
                    |> Sihl.Web.externalize_path)
                ]
              [ Component.csrf_element csrf ()
              ; submit_element language Message.(Stop None) ()
              ]
          ]
      ; td
          [ form
              ~a:
                [ a_method `Post
                ; a_action
                    (Format.asprintf
                       "/admin/experiments/%s/Mailings/%s/delete"
                       (Pool_common.Id.value experiment_id)
                       (Id.value mailing.id)
                    |> Sihl.Web.externalize_path)
                ]
              [ Component.csrf_element csrf ()
              ; submit_element
                  language
                  Message.(Delete None)
                  ~submit_type:`Error
                  ()
              ]
          ]
      ]
  in
  let thead =
    let open Pool_common in
    Message.Field.[ Some Start; Some End; Some Rate; None; None; None ]
    |> CCList.map (fun field ->
           th
             [ txt
                 (CCOption.map_or
                    ~default:""
                    (fun f -> Utils.field_to_string_capitalized language f)
                    field)
             ])
    |> tr
    |> CCList.pure
    |> thead
  in
  let html =
    div
      ~a:[ a_class [ "stack-lg" ] ]
      [ table
          ~thead
          ~a:[ a_class [ "striped" ] ]
          (CCList.map mailing_row mailings)
      ]
  in
  Page_admin_experiments.experiment_layout
    language
    (Page_admin_experiments.NavLink Pool_common.I18n.Mailings)
    experiment.Experiment.id
    ~active:Pool_common.I18n.Mailings
    html
;;

let detail Pool_context.{ language; _ } experiment_id (mailing : Mailing.t) =
  let open Mailing in
  let mailing_overview =
    div
      ~a:[ a_class [ "stack" ] ]
      [ (* TODO [aerben] use better formatted date *)
        (let rows =
           let open Message in
           [ Field.Start, mailing.start_at |> StartAt.to_human
           ; Field.End, mailing.end_at |> EndAt.to_human
           ; Field.Rate, mailing.rate |> Rate.value |> CCInt.to_string
           ; ( Field.Distribution
             , mailing.distribution |> Mailing.Distribution.show )
           ]
           |> CCList.map (fun (field, value) ->
                  tr
                    [ th
                        [ txt
                            (field
                            |> Pool_common.Utils.field_to_string language
                            |> CCString.capitalize_ascii)
                        ]
                    ; td [ txt value ]
                    ])
         in
         table ~a:[ a_class [ "striped" ] ] rows)
      ; p
          [ a
              ~a:
                [ a_href
                    (Format.asprintf
                       "/admin/experiments/%s/mailings/%s/edit"
                       (Pool_common.Id.value experiment_id)
                       (Id.value mailing.id)
                    |> Sihl.Web.externalize_path)
                ]
              [ Message.(Edit (Some Field.Mailing))
                |> Pool_common.Utils.control_to_string language
                |> txt
              ]
          ]
      ]
  in
  let html = div ~a:[ a_class [ "stack-lg" ] ] [ mailing_overview ] in
  Page_admin_experiments.experiment_layout
    language
    (Page_admin_experiments.I18n (mailing_title mailing))
    experiment_id
    html
;;

let form
    ?(mailing : Mailing.t option)
    Pool_context.{ language; csrf; _ }
    experiment_id
    flash_fetcher
  =
  let heading, action, submit =
    match mailing with
    | None ->
      let text = Message.(Create (Some Field.Mailing)) in
      ( text |> Pool_common.Utils.control_to_string language
      , Format.asprintf "/admin/experiments/%s/mailings"
        @@ Pool_common.Id.value experiment_id
      , text )
    | Some ({ Mailing.id; _ } as m) ->
      ( m |> mailing_title |> Pool_common.Utils.text_to_string language
      , Format.asprintf
          "/admin/experiments/%s/mailings/%s"
          (Pool_common.Id.value experiment_id)
          (Mailing.Id.value id)
      , Message.(Edit (Some Field.Mailing)) )
  in
  let html =
    div
      [ h1 ~a:[ a_class [ "heading-2" ] ] [ txt heading ]
      ; form
          ~a:
            [ a_class [ "stack" ]
            ; a_method `Post
            ; a_action (action |> Sihl.Web.externalize_path)
            ]
          [ Component.csrf_element csrf ()
          ; input_element
              ?value:
                (CCOption.map
                   (fun (m : Mailing.t) ->
                     m.Mailing.start_at
                     |> Mailing.StartAt.value
                     |> Ptime.to_rfc3339 ~space:true)
                   mailing)
              language
              `Datetime
              Pool_common.Message.Field.Start
              ~flash_fetcher
          ; input_element
              ?value:
                (CCOption.map
                   (fun (m : Mailing.t) ->
                     m.Mailing.end_at
                     |> Mailing.EndAt.value
                     |> Ptime.to_rfc3339 ~space:true)
                   mailing)
              language
              `Datetime
              Pool_common.Message.Field.End
              ~flash_fetcher
          ; input_element
              ?value:
                (CCOption.map
                   (fun (m : Mailing.t) ->
                     m.Mailing.rate |> Mailing.Rate.value |> CCInt.to_string)
                   mailing)
              language
              `Number
              Pool_common.Message.Field.Rate
              ~flash_fetcher
          ; input_element
              language
              `Text
              Pool_common.Message.Field.Distribution
              ~flash_fetcher
              ?value:
                (CCOption.map
                   (fun (m : Mailing.t) ->
                     Yojson.Safe.to_string
                       Mailing.Distribution.(
                         m.Mailing.distribution |> create |> yojson_of_t))
                   mailing)
            (* TODO: update Distribution element *)
          ; submit_element language submit ()
          ]
      ]
  in
  Page_admin_experiments.experiment_layout
    language
    (Page_admin_experiments.Control submit)
    experiment_id
    html
;;

let create context experiment_id flash_fetcher =
  form context experiment_id flash_fetcher
;;

let edit context experiment_id mailing flash_fetcher =
  form ~mailing context experiment_id flash_fetcher
;;
