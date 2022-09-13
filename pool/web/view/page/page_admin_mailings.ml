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

let distribution_sort_select language ?field current_order =
  let open Mailing.Distribution.SortOrder in
  let select_name =
    let open Pool_common.Message in
    match field with
    | None -> Field.(show SortOrder)
    | Some _ -> Field.(array_key Distribution)
  in
  CCList.map
    (fun order ->
      let selected =
        match equal order current_order with
        | true -> [ a_selected () ]
        | false -> []
      in
      option
        ~a:
          ([ a_value
               (match field with
                | None -> order |> show
                | Some field ->
                  Format.asprintf
                    "%s,%s"
                    (Pool_common.Message.Field.show field)
                    (order |> show))
           ]
          @ selected)
        (order
        |> CCFun.flip to_human language
        |> CCString.capitalize_ascii
        |> txt))
    all
  |> fun options ->
  div ~a:[ a_class [ "select" ] ] [ select ~a:[ a_name select_name ] options ]
;;

let distribution_form_field language (field, current_order) =
  div
    ~a:
      [ a_class [ "flexrow"; "flex-gap"; "distribution" ]
      ; a_user_data "sortable-item" ""
      ; a_draggable true
      ]
    [ div
        ~a:[ a_class [ "switcher"; "flex-gap"; "align-center"; "grow" ] ]
        [ label
            [ Pool_common.(Utils.field_to_string language field)
              |> CCString.capitalize_ascii
              |> txt
            ]
        ; div
            ~a:[ a_class [ "form-group" ] ]
            [ distribution_sort_select language ~field current_order ]
        ]
    ; div
        [ button
            ~a:
              [ a_class [ "error" ]
              ; a_onclick "removeDistribution(event)"
              ; a_button_type `Button
              ; a_user_data "field" Pool_common.Message.Field.(show field)
              ]
            [ Component.Icon.icon `Trash ]
        ]
    ]
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
                (* TODO [aerben] this is wrong, should be plural, add a Plural
                   constructor?*)
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
    experiment
    ~active:I18n.Mailings
    html
;;

let detail Pool_context.{ language; _ } experiment (mailing : Mailing.t) =
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
                  [ detail_mailing_path
                      ~suffix:"edit"
                      experiment.Experiment.id
                      mailing
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
    experiment
    html
;;

let form
  ?(mailing : Mailing.t option)
  Pool_context.{ language; csrf; _ }
  experiment
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
  let distribution_select (distribution : Mailing.Distribution.t option) =
    let open Mailing.Distribution in
    let open Pool_common.Message in
    let is_disabled field =
      CCOption.map_or
        ~default:false
        (fun dist -> CCList.mem_assoc ~eq:Field.equal field dist)
        distribution
    in
    let distribution_fncs =
      {js|
        function removeDistribution(e) {
          e.preventDefault();
          var field = e.currentTarget.dataset.field;
          var select = document.getElementById('distribution-select');
          var options = Array.from(select.getElementsByTagName('option'));
          options.forEach(option => {
            if (option.value === field) {
                option.disabled = false;
            }
          })
          e.currentTarget.closest('.distribution').remove();
        }

      document.querySelector('#distribution-list').addEventListener('htmx:beforeSwap', (e) => {
        var param = e.detail.requestConfig.parameters.distribution_field;
        var select = document.getElementById('distribution-select');
        var options = Array.from(select.getElementsByTagName('option'));
        var defaultOption = options.find((elm) => !elm.value);
        if (param) {
          options.forEach(option => {
            if (option.value === param) {
              option.disabled = true;
            }
            option.selected = false;
            })
          defaultOption.selected = true;
        } else {
          e.detail.shouldSwap = false
        }
      })
    |js}
    in
    let field_select =
      let default_option =
        option
          ~a:[ a_value ""; a_disabled (); a_selected () ]
          (Pool_common.(Utils.control_to_string language Message.PleaseSelect)
          |> CCString.capitalize_ascii
          |> txt)
      in
      CCList.map
        (fun field ->
          let is_disabled =
            if is_disabled field then [ a_disabled () ] else []
          in
          option
            ~a:([ a_value (field |> Field.show) ] @ is_disabled)
            (field
            |> Pool_common.Utils.field_to_string language
            |> CCString.capitalize_ascii
            |> txt))
        sortable_fields
      |> fun options ->
      select
        ~a:
          [ a_id "distribution-select"
          ; a_name Pool_common.Message.Field.(show DistributionField)
          ]
        (default_option :: options)
    in
    let sort_select = distribution_sort_select language SortOrder.default in
    let form_group select field =
      div
        ~a:[ a_class [ "form-group" ] ]
        [ label
            [ txt
                (Pool_common.(Utils.field_to_string language field)
                |> CCString.capitalize_ascii)
            ]
        ; div ~a:[ a_class [ "select" ] ] [ select ]
        ]
    in
    div
      ~a:[ a_class [ "flexcolumn" ] ]
      [ h3
          [ txt
              (Pool_common.(
                 Utils.field_to_string language Message.Field.Distribution)
              |> CCString.capitalize_ascii)
          ]
      ; p [ txt Pool_common.(Utils.hint_to_string language I18n.Distribution) ]
      ; div
          ~a:
            [ a_class
                [ "border-bottom"
                ; "inset"
                ; "u-shape"
                ; "vertical"
                ; "flexrow"
                ; "flex-gap"
                ]
            ]
          [ div
              ~a:[ a_class [ "switcher"; "flex-gap"; "grow" ] ]
              [ form_group field_select Message.Field.DistributionField
              ; form_group sort_select Message.Field.SortOrder
              ]
          ; div
              ~a:[ a_class [ "form-group"; "justify-end" ] ]
              [ button
                  ~a:
                    [ a_class [ "success" ]
                    ; a_user_data
                        "hx-post"
                        (mailings_path
                           ~suffix:"add-condition"
                           experiment.Experiment.id)
                    ; a_user_data "hx-trigger" "click"
                    ; a_user_data "hx-target" "#distribution-list"
                    ; a_user_data "hx-swap" "beforeend"
                    ]
                  [ Component.Icon.icon `Add ]
              ]
          ]
      ; div
          [ div
              ~a:
                [ a_id "distribution-list"
                ; a_class [ "gap"; "flexcolumn" ]
                ; a_user_data "sortable" ""
                ]
              (CCOption.map_or
                 ~default:[]
                 (fun distribution ->
                   CCList.map (distribution_form_field language) distribution)
                 distribution)
          ; script (Unsafe.data distribution_fncs)
          ]
      ]
  in
  let action, submit =
    match mailing with
    | None ->
      ( mailings_path experiment.Experiment.id
      , Message.(Create (Some Field.Mailing)) )
    | Some m ->
      ( m |> detail_mailing_path experiment.Experiment.id
      , Message.(Edit (Some Field.Mailing)) )
  in
  let html =
    let open Htmx in
    div
      ~a:[ a_class [ "stack" ] ]
      [ form
          ~a:[ a_class [ "stack" ]; a_method `Post; a_action action ]
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
                ; hx_swap "innerHTML"
                ; hx_post
                    (mailings_path
                       ~suffix:"search-info"
                       experiment.Experiment.id)
                ]
              [ flatpicker_element
                  language
                  `Datetime_local
                  Field.Start
                  ~flash_fetcher
                  ~required:true
                  ~disable_past:true
                  ?value:
                    (CCOption.map
                       (fun (m : Mailing.t) ->
                         m.Mailing.start_at
                         |> Mailing.StartAt.value
                         |> Ptime.to_rfc3339 ~space:true)
                       mailing)
              ; flatpicker_element
                  language
                  `Datetime_local
                  Field.End
                  ~flash_fetcher
                  ~disable_past:true
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
                  ~additional_attributes:[ a_input_min (`Number 1) ]
              ]
          ; distribution_select
              (CCOption.bind mailing (fun (m : Mailing.t) ->
                 m.Mailing.distribution))
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
    experiment
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
  div ~a:[ a_class [ "stack" ] ] (average @ total @ mailings)
;;
