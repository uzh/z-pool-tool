open Tyxml.Html
open Component
module Message = Pool_common.Message
module Field = Message.Field
module I18n = Pool_common.I18n

let mailing_title (s : Mailing.t) =
  I18n.MailingDetailTitle (s.Mailing.start_at |> Mailing.StartAt.value)
;;

let mailings_path ?suffix experiment_id =
  [ Format.asprintf
      "/admin/experiments/%s/mailings"
      (Pool_common.Id.value experiment_id)
  ]
  @ CCOption.map_or ~default:[] CCList.pure suffix
  |> CCString.concat "/"
  |> Sihl.Web.externalize_path
;;

let detail_mailing_path ?suffix experiment_id mailing =
  let open Mailing in
  [ mailings_path ~suffix:(Id.value mailing.id) experiment_id ]
  @ CCOption.map_or ~default:[] CCList.pure suffix
  |> CCString.concat "/"
;;

module List = struct
  let row
      with_link
      Pool_context.{ csrf; language; _ }
      experiment_id
      (mailing : Mailing.t)
    =
    let open Mailing in
    let now = Ptime_clock.now () in
    let button_form target name submit_type confirm_text =
      form
        ~a:
          [ a_method `Post
          ; a_action (detail_mailing_path ~suffix:target experiment_id mailing)
          ; a_user_data
              "confirmable"
              (Pool_common.Utils.confirmable_to_string language confirm_text)
          ]
        [ Component.csrf_element csrf ()
        ; submit_element ~submit_type language (name None) ()
        ]
    in
    [ mailing.start_at |> StartAt.to_human |> txt
    ; mailing.end_at |> EndAt.to_human |> txt
    ; mailing.rate |> Rate.value |> CCInt.to_string |> txt
    ; a
        ~a:[ detail_mailing_path experiment_id mailing |> a_href ]
        [ txt Pool_common.(Utils.control_to_string language Message.(More)) ]
    ]
    @
    if with_link
    then (
      match
        StartAt.value mailing.start_at < now, now < EndAt.value mailing.end_at
      with
      | true, true ->
        [ button_form "stop" Message.stop `Primary I18n.StopMailing ]
      | false, true ->
        [ button_form "delete" Message.delete `Error I18n.DeleteMailing ]
      | _ -> [ txt "" ])
    else []
  ;;

  let create
      with_link
      (Pool_context.{ language; _ } as context)
      experiment_id
      mailings
    =
    let base_head = Field.[ Some Start; Some End; Some Rate; None ] in
    let thead = if with_link then base_head @ [ None ] else base_head in
    Component.Table.(horizontal_table `Striped language ~thead)
      (CCList.map (row with_link context experiment_id) mailings)
  ;;
end

let index (Pool_context.{ language; _ } as context) experiment mailings =
  let experiment_id = experiment.Experiment.id in
  let open Pool_common in
  let html =
    div
      ~a:[ a_class [ "stack" ] ]
      [ a
          ~a:[ mailings_path ~suffix:"create" experiment_id |> a_href ]
          [ Message.(Add (Some Field.Mailing))
            |> Utils.control_to_string language
            |> txt
          ]
      ; (if CCList.is_empty mailings
        then
          div
            [ p
                [ I18n.EmtpyList Field.Mailing
                  |> Utils.text_to_string language
                  |> txt
                ]
            ]
        else List.create true context experiment_id mailings)
      ]
  in
  Page_admin_experiments.experiment_layout
    language
    (Page_admin_experiments.NavLink I18n.Mailings)
    experiment.Experiment.id
    ~active:I18n.Mailings
    html
;;

let detail Pool_context.{ language; _ } experiment_id (mailing : Mailing.t) =
  let open Mailing in
  let mailing_overview =
    div
      ~a:[ a_class [ "stack" ] ]
      ([ (* TODO [aerben] use better formatted date *)
         (let rows =
            let open Message in
            [ Field.Start, mailing.start_at |> StartAt.to_human
            ; Field.End, mailing.end_at |> EndAt.to_human
            ; Field.Rate, mailing.rate |> Rate.value |> CCInt.to_string
            ; ( Field.Distribution
              , mailing.distribution
                |> CCOption.map_or ~default:"" Mailing.Distribution.show )
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
          table ~a:[ a_class [ "striped"; "table" ] ] rows)
       ]
      @
      if StartAt.value mailing.start_at > Ptime_clock.now ()
      then
        [ p
            [ a
                ~a:
                  [ detail_mailing_path ~suffix:"edit" experiment_id mailing
                    |> a_href
                  ]
                [ Message.(Edit (Some Field.Mailing))
                  |> Pool_common.Utils.control_to_string language
                  |> txt
                ]
            ]
        ]
      else [])
  in
  let html = div ~a:[ a_class [ "stack" ] ] [ mailing_overview ] in
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
  let functions =
    {js|
      var container = document.getElementById('mailings');
      container.addEventListener('htmx:beforeRequest', (e) => {
        var start = container.querySelector("[name='start']").value;
        var end = container.querySelector("[name='end']").value;

        if ((!start || !end) || Date.parse(start) > Date.parse(end)) {
          e.preventDefault();
          };
      });
  |js}
  in
  let action, submit =
    match mailing with
    | None -> mailings_path experiment_id, Message.(Create (Some Field.Mailing))
    | Some m ->
      ( m |> detail_mailing_path experiment_id
      , Message.(Edit (Some Field.Mailing)) )
  in
  let html =
    let open Htmx in
    div
      ~a:[ a_class [ "stack" ] ]
      [ form
          ~a:
            [ a_class [ "stack" ]
            ; a_method `Post
            ; a_action (action |> Sihl.Web.externalize_path)
            ]
          [ Component.csrf_element csrf ()
          ; input
              ~a:
                [ a_input_type `Hidden
                ; a_name "id"
                ; a_value
                    (CCOption.map_or
                       ~default:""
                       (fun m -> m.Mailing.id |> Mailing.Id.value)
                       mailing)
                ]
              ()
          ; div
              ~a:
                [ a_id "mailings"
                ; a_class [ "stack" ]
                ; hx_target "#overlaps"
                ; hx_trigger "change"
                ; hx_swap "outerHTML"
                ; hx_post (mailings_path ~suffix:"search-info" experiment_id)
                ]
              [ input_element
                  language
                  `Datetime
                  Field.Start
                  ~flash_fetcher
                  ~required:true
                  ?value:
                    (CCOption.map
                       (fun (m : Mailing.t) ->
                         m.Mailing.start_at
                         |> Mailing.StartAt.value
                         |> Ptime.to_rfc3339 ~space:true)
                       mailing)
              ; input_element
                  language
                  `Datetime
                  Field.End
                  ~flash_fetcher
                  ~required:true
                  ?value:
                    (CCOption.map
                       (fun (m : Mailing.t) ->
                         m.Mailing.end_at
                         |> Mailing.EndAt.value
                         |> Ptime.to_rfc3339 ~space:true)
                       mailing)
              ; input_element
                  language
                  `Number
                  Field.Rate
                  ~flash_fetcher
                  ~required:true
                  ~help:I18n.Rate
                  ~value:
                    (mailing
                    |> CCOption.map_or
                         ~default:Mailing.Rate.default
                         (fun (m : Mailing.t) -> m.Mailing.rate)
                    |> Mailing.Rate.value
                    |> CCInt.to_string)
              ]
          ; input_element
              language
              `Text
              Field.Distribution
              ~flash_fetcher
              ~help:I18n.Distribution
              ?value:
                CCOption.(
                  mailing
                  >>= (fun m -> m.Mailing.distribution)
                  >|= fun m ->
                  m |> Mailing.Distribution.yojson_of_t |> Yojson.Safe.to_string)
            (* TODO: Add detailed description how distribution element works *)
          ; submit_element language submit ()
          ]
      ; div ~a:[ a_id "overlaps" ] []
      ; script (Unsafe.data functions)
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

let overlaps
    ?average_send
    ?total
    (Pool_context.{ language; _ } as context)
    experiment_id
    mailings
  =
  let average =
    match average_send with
    | None -> []
    | Some average ->
      [ p
          [ Pool_common.(
              I18n.RateNumberPerMinutes (5, average)
              |> Utils.hint_to_string language)
            |> txt
          ]
      ]
  in
  let total =
    match total with
    | None -> []
    | Some total ->
      [ p
          [ I18n.RateTotalSent total
            |> Pool_common.Utils.text_to_string language
            |> txt
          ]
      ]
  in
  let mailings =
    match CCList.is_empty mailings with
    | true ->
      [ p
          [ I18n.RateDependencyWithout
            |> Pool_common.Utils.hint_to_string language
            |> txt
          ]
      ]
    | false ->
      [ p
          [ I18n.RateDependencyWith
            |> Pool_common.Utils.hint_to_string language
            |> txt
          ]
      ; List.create false context experiment_id mailings
      ]
  in
  div ~a:[ a_class [ "stack" ]; a_id "overlaps" ] (average @ total @ mailings)
;;
