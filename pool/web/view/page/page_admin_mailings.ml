open Tyxml.Html
open Component
open Input
module Message = Pool_common.Message
module Field = Message.Field
module I18n = Pool_common.I18n

let mailing_title (s : Mailing.t) =
  I18n.MailingDetailTitle (s.Mailing.start_at |> Mailing.StartAt.value)
;;

let append_suffied suffix path =
  match suffix with
  | None -> path
  | Some suffix -> Format.asprintf "%s/%s" path suffix
;;

let mailings_path ?suffix experiment_id =
  Format.asprintf
    "/admin/experiments/%s/mailings"
    (Experiment.Id.value experiment_id)
  |> append_suffied suffix
;;

let detail_mailing_path ?suffix experiment_id mailing =
  let open Mailing in
  mailings_path ~suffix:(Id.value mailing.id) experiment_id
  |> append_suffied suffix
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
                    (Mailing.Distribution.SortableField.show field)
                    (order |> show))
           ]
           @ selected)
        (order |> to_human language |> CCString.capitalize_ascii |> txt))
    all
  |> fun options ->
  div ~a:[ a_class [ "select" ] ] [ select ~a:[ a_name select_name ] options ]
;;

let distribution_form_field language (field, current_order) =
  div
    ~a:
      [ a_class [ "flexrow"; "flex-gap"; "distribution"; "inset-sm" ]
      ; a_user_data "sortable-item" ""
      ]
    [ div
        ~a:[ a_class [ "switcher"; "flex-gap"; "align-center"; "grow" ] ]
        [ label
            [ Mailing.Distribution.SortableField.to_human language field
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
              ; a_user_data
                  "field"
                  (Mailing.Distribution.SortableField.show field)
              ]
            Icon.[ to_html Trash ]
        ]
    ]
;;

module List = struct
  type mailing =
    | Standard of Mailing.t list
    | Counted of (Mailing.t * Mailing.InvitationCount.t) list

  let buttons with_link experiment_id mailing language csrf =
    let open Mailing in
    let now = Ptime_clock.now () in
    let button_form target name submit_type confirm_text =
      form
        ~a:
          [ a_method `Post
          ; a_action
              (detail_mailing_path ~suffix:target experiment_id mailing
               |> Sihl.Web.externalize_path)
          ; a_user_data
              "confirmable"
              (Pool_common.Utils.confirmable_to_string language confirm_text)
          ]
        [ csrf_element csrf ()
        ; submit_element ~submit_type language (name None) ()
        ]
    in
    (if with_link
     then (
       match
         StartAt.value mailing.start_at < now, now < EndAt.value mailing.end_at
       with
       | true, true ->
         [ button_form "stop" Message.stop `Primary I18n.StopMailing ]
       | false, true ->
         [ button_form "delete" Message.delete `Error I18n.DeleteMailing ]
       | _ -> [ txt "" ])
     else [])
    @ [ detail_mailing_path experiment_id mailing
        |> link_as_button ~icon:Icon.Eye
      ]
    |> div ~a:[ a_class [ "flexrow"; "flex-gap"; "justify-end" ] ]
  ;;

  let row_with_count
    with_link
    Pool_context.{ csrf; language; _ }
    experiment_id
    ((mailing, count) : Mailing.t * Mailing.InvitationCount.t)
    =
    let open Mailing in
    let buttons = buttons with_link experiment_id mailing language csrf in
    [ mailing.start_at |> StartAt.to_human |> txt
    ; mailing.end_at |> EndAt.to_human |> txt
    ; mailing.limit |> Limit.value |> CCInt.to_string |> txt
    ; count |> InvitationCount.value |> CCInt.to_string |> txt
    ; buttons
    ]
  ;;

  let row
    with_link
    Pool_context.{ csrf; language; _ }
    experiment_id
    (mailing : Mailing.t)
    =
    let open Mailing in
    let buttons = buttons with_link experiment_id mailing language csrf in
    [ mailing.start_at |> StartAt.to_human |> txt
    ; mailing.end_at |> EndAt.to_human |> txt
    ; mailing.limit |> Limit.value |> CCInt.to_string |> txt
    ; buttons
    ]
  ;;

  let create
    with_link
    (Pool_context.{ language; _ } as context)
    experiment_id
    mailings
    =
    let base_head =
      Table.fields_to_txt language
      @@
      match mailings with
      | Standard _ -> Field.[ Start; End; Limit ]
      | Counted _ -> Field.[ Start; End; Limit; InvitationCount ]
    in
    let thead =
      let new_btn () =
        link_as_button
          ~style:`Success
          ~icon:Icon.Add
          ~control:(language, Message.Add (Some Field.Mailing))
          (mailings_path ~suffix:"create" experiment_id)
      in
      if with_link then base_head @ [ new_btn () ] else base_head
    in
    Table.(horizontal_table `Striped ~align_last_end:true ~thead)
    @@
    match mailings with
    | Standard mailings ->
      CCList.map (row with_link context experiment_id) mailings
    | Counted mailings ->
      CCList.map (row_with_count with_link context experiment_id) mailings
  ;;
end

let index context experiment mailings =
  let experiment_id = experiment.Experiment.id in
  let open Pool_common in
  List.(create true context experiment_id (Counted mailings))
  |> CCList.return
  |> Layout.Experiment.(
       create
         ~active_navigation:I18n.Mailings
         ~hint:I18n.ExperimentMailings
         context
         (NavLink I18n.Mailings)
         experiment)
;;

let detail
  ({ Pool_context.language; _ } as context)
  experiment
  ((mailing, count) : Mailing.t * Mailing.InvitationCount.t)
  =
  let open Mailing in
  let mailing_overview =
    div
      ~a:[ a_class [ "stack" ] ]
      [ (* TODO [aerben] use better formatted date *)
        (let rows =
           let open Message in
           [ Field.Start, mailing.start_at |> StartAt.to_human
           ; Field.End, mailing.end_at |> EndAt.to_human
           ; Field.Limit, mailing.limit |> Limit.value |> CCInt.to_string
           ; ( Field.InvitationCount
             , count |> InvitationCount.value |> CCInt.to_string )
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
  in
  let edit_button =
    if StartAt.value mailing.start_at > Ptime_clock.now ()
    then
      link_as_button
        ~icon:Icon.Create
        ~classnames:[ "small" ]
        ~control:(language, Message.Edit (Some Field.Mailing))
        (detail_mailing_path ~suffix:"edit" experiment.Experiment.id mailing)
    else txt ""
  in
  div ~a:[ a_class [ "stack" ] ] [ mailing_overview ]
  |> CCList.return
  |> Layout.Experiment.(
       create
         ~buttons:edit_button
         context
         (I18n (mailing_title mailing))
         experiment)
;;

let form
  ?(mailing : Mailing.t option)
  ?(fully_booked = false)
  ({ Pool_context.language; csrf; _ } as context)
  (experiment : Experiment.t)
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
  let notification =
    match
      Experiment.(
        experiment.registration_disabled |> RegistrationDisabled.value)
    with
    | true ->
      txt
        Pool_common.(
          Utils.hint_to_string
            language
            I18n.ExperimentMailingsRegistrationDisabled)
      |> fun text ->
      [ p [ text ] ] |> Component.Notification.notification language `Warning
    | false -> txt ""
  in
  let distribution_select (distribution : Mailing.Distribution.t option) =
    let open Mailing.Distribution in
    let subform_id = "distribution-subform" in
    let is_disabled field =
      CCOption.map_or
        ~default:false
        (fun dist ->
          dist |> find_dist |> CCList.mem_assoc ~eq:SortableField.equal field)
        distribution
    in
    let distribution_fncs =
      let open Field in
      Format.asprintf
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

      const random = document.querySelector('[name="%s"]');
      const subform = document.getElementById('%s')
      const inputs = subform.querySelectorAll('input, select, button');
      const toggleRandomOrder = () => {
        inputs.forEach((elm) => {
          elm.disabled = random.checked
        })
        if(random.checked) {
          subform.classList.add("hidden");
        } else {
          subform.classList.remove("hidden");
        }
      }

      random.addEventListener("change", (e) => {
        toggleRandomOrder();
      })

      const startNow = document.querySelector('[name="%s"]');
      startNow.addEventListener("change", (e) => {
        const startSelect = '[name="%s"]';
        const start = document.querySelector(`${startSelect}`);
        const startFlatpicker = document.querySelector(`${startSelect} + input.datepicker`);
        start.required = !startNow.checked;
        startFlatpicker.required = !startNow.checked;
        start.disabled = startNow.checked;
        startFlatpicker.disabled = startNow.checked;
      })
      toggleRandomOrder();
    |js}
        (show RandomOrder)
        subform_id
        (show StartNow)
        (show Start)
    in
    let field_select =
      let default_option =
        option
          ~a:[ a_value ""; a_disabled (); a_selected () ]
          (Pool_common.(Utils.control_to_string language Message.PleaseSelect)
           |> txt)
      in
      CCList.map
        (fun field ->
          let is_disabled =
            if is_disabled field then [ a_disabled () ] else []
          in
          option
            ~a:([ a_value (field |> SortableField.show) ] @ is_disabled)
            (field
             |> SortableField.to_human language
             |> CCString.capitalize_ascii
             |> txt))
        SortableField.all
      |> fun options ->
      select
        ~a:[ a_id "distribution-select"; a_name Field.(show DistributionField) ]
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
    let random_is_checked =
      match mailing with
      | None -> true
      | Some mailing ->
        mailing.Mailing.distribution
        |> CCOption.map Mailing.Distribution.is_random
        |> CCOption.value ~default:false
    in
    div
      ~a:[ a_class [ "flexcolumn" ] ]
      [ h3
          [ txt
              (Pool_common.(Utils.field_to_string language Field.Distribution)
               |> CCString.capitalize_ascii)
          ]
      ; p [ txt Pool_common.(Utils.hint_to_string language I18n.Distribution) ]
      ; checkbox_element
          ~value:random_is_checked
          ~flash_fetcher
          language
          Field.RandomOrder
      ; div
          ~a:
            [ a_id subform_id
            ; a_class (if random_is_checked then [ "hidden" ] else [])
            ]
          [ div
              ~a:
                [ a_class
                    [ "border-bottom"
                    ; "inset"
                    ; "u-shape"
                    ; "vertical"
                    ; "flexrow"
                    ; "flex-gap"
                    ; "gap"
                    ]
                ; a_id subform_id
                ]
              [ div
                  ~a:[ a_class [ "switcher"; "flex-gap"; "grow" ] ]
                  [ form_group field_select Field.DistributionField
                  ; form_group sort_select Field.SortOrder
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
                               experiment.Experiment.id
                             |> Sihl.Web.externalize_path)
                        ; a_user_data "hx-trigger" "click"
                        ; a_user_data "hx-target" "#distribution-list"
                        ; a_user_data "hx-swap" "beforeend"
                        ]
                      Icon.[ to_html Add ]
                  ]
              ]
          ; div
              [ Mailing.Distribution.(
                  distribution |> CCOption.map_or ~default:[] find_dist)
                |> CCList.map (distribution_form_field language)
                |> Component.Sortable.create
                     ~classnames:[ "flexcolumn" ]
                     ~attributes:[ a_id "distribution-list" ]
                |> CCList.pure
                |> div ~a:[ a_class [ "gap" ] ]
              ; script (Unsafe.data distribution_fncs)
              ]
          ]
      ]
  in
  let fully_booked_note =
    if fully_booked
    then
      [ div
          ~a:
            [ a_class [ "bg-grey-light"; "border"; "border-radius"; "inset-md" ]
            ]
          [ Pool_common.Utils.text_to_string
              language
              I18n.MailingExperimentSessionFullyBooked
            |> HttpUtils.add_line_breaks
          ]
      ]
    else []
  in
  let action, submit =
    match mailing with
    | None ->
      ( mailings_path experiment.Experiment.id
      , Message.Create (Some Field.Mailing) )
    | Some m ->
      ( m |> detail_mailing_path experiment.Experiment.id
      , Message.Save (Some Field.Mailing) )
  in
  let html =
    let open Htmx in
    div
      ~a:[ a_class [ "stack" ] ]
      ((notification :: fully_booked_note)
       @ [ form
             ~a:
               [ a_class [ "stack" ]
               ; a_method `Post
               ; a_action (action |> Sihl.Web.externalize_path)
               ; a_user_data "detect-unsaved-changes" ""
               ]
             [ csrf_element csrf ()
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
                   ; a_class [ "grid-col-2" ]
                   ; hx_target "#overlaps"
                   ; hx_trigger "change"
                   ; hx_swap "innerHTML"
                   ; hx_post
                       (mailings_path
                          ~suffix:"search-info"
                          experiment.Experiment.id
                        |> Sihl.Web.externalize_path)
                   ]
                 [ div
                     ~a:[ a_class [ "flexcolumn" ] ]
                     [ date_time_picker_element
                         language
                         Field.Start
                         ~flash_fetcher
                         ~required:true
                         ~disable_past:true
                         ?value:
                           (CCOption.map
                              (fun (m : Mailing.t) ->
                                m.Mailing.start_at |> Mailing.StartAt.value)
                              mailing)
                     ; checkbox_element ~flash_fetcher language Field.StartNow
                     ]
                 ; date_time_picker_element
                     language
                     Field.End
                     ~flash_fetcher
                     ~disable_past:true
                     ~required:true
                     ?value:
                       (CCOption.map
                          (fun (m : Mailing.t) ->
                            m.Mailing.end_at |> Mailing.EndAt.value)
                          mailing)
                 ; input_element
                     language
                     `Number
                     Field.Limit
                     ~flash_fetcher
                     ~required:true
                     ~help:I18n.MailingLimit
                     ~value:
                       Mailing.(
                         mailing
                         |> CCOption.map_or
                              ~default:Limit.default
                              (fun (m : t) -> m.limit)
                         |> Limit.value
                         |> CCInt.to_string)
                     ~additional_attributes:[ a_input_min (`Number 1) ]
                 ]
             ; distribution_select
                 (CCOption.bind mailing (fun (m : Mailing.t) ->
                    m.Mailing.distribution))
               (* TODO: Add detailed description how distribution element
                  works *)
             ; div
                 ~a:[ a_class [ "flexrow" ] ]
                 [ submit_element ~classnames:[ "push" ] language submit () ]
             ]
         ; div ~a:[ a_id "overlaps" ] []
         ; script (Unsafe.data functions)
         ])
    |> CCList.return
  in
  Layout.Experiment.(create context (Control submit) experiment html)
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
          [ I18n.RateNumberPerMinutes (5, average)
            |> Pool_common.Utils.hint_to_string language
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
      ; List.(create false context experiment_id (Standard mailings))
      ]
  in
  div ~a:[ a_class [ "stack" ] ] (average @ total @ mailings)
;;
