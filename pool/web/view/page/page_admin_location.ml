open CCFun
open Containers
open Tyxml.Html
open Component.Input
module Message = Pool_message

let base_path = "/admin/locations"

let location_specific_path ?suffix id =
  let path = Format.asprintf "%s/%s" base_path (Pool_location.Id.value id) in
  match suffix with
  | None -> path
  | Some suffix -> Format.asprintf "%s/%s" path suffix
;;

let make_statistics ?year year_range language location_id t =
  let open Pool_location in
  let open Statistics in
  let int_to_txt i = i |> CCInt.to_string |> txt in
  let id = Format.asprintf "location-%s" (Id.value location_id) in
  let table =
    [ ExperimentCount.(field, t |> experiment_count |> value |> int_to_txt)
    ; AssignmentCount.(field, t |> assignment_count |> value |> int_to_txt)
    ; ShowUpCount.(field, t |> showup_count |> value |> int_to_txt)
    ; NoShowCount.(field, t |> noshow_count |> value |> int_to_txt)
    ; ParticipationCount.(
        field, t |> participation_count |> value |> int_to_txt)
    ]
    |> Component.Table.vertical_table ~classnames:[ "fixed" ] `Striped language
  in
  let htmx =
    Htmx.
      [ hx_get
          (location_id
           |> location_specific_path ~suffix:"statistics"
           |> Sihl.Web.externalize_path)
      ; hx_trigger "change"
      ; hx_target ("#" ^ id)
      ; hx_swap "outerHTML"
      ]
  in
  div
    ~a:[ a_id id ]
    [ h3
        [ Pool_common.(Utils.text_to_string language I18n.LocationStatistics)
          |> txt
        ]
    ; div
        ~a:[ a_class [ "stack" ] ]
        [ selector
            ~attributes:htmx
            language
            Pool_message.Field.Year
            CCInt.to_string
            year_range
            year
            ()
        ; table
        ]
    ]
;;

let descriptions_all_languages (location : Pool_location.t) =
  let open Pool_location in
  location.description
  |> CCOption.map (fun desc ->
    desc
    |> Description.value
    |> CCList.map (fun (lang, value) ->
      div
        [ strong [ txt Pool_common.Language.(show lang) ]
        ; div [ Unsafe.data value ]
        ]))
  |> CCOption.value ~default:[]
  |> Utils.Html.concat_html
  |> div
;;

let list Pool_context.{ language; _ } location_list query =
  let url = Uri.of_string base_path in
  let data_table =
    Component.DataTable.create_meta
      ~search:Pool_location.searchable_by
      url
      query
      language
  in
  let cols =
    let create_filter : [ | Html_types.flow5 ] elt =
      Component.Input.link_as_button
        ~style:`Success
        ~icon:Component.Icon.Add
        ~classnames:[ "small"; "nobr" ]
        ~control:(language, Pool_message.(Control.Add (Some Field.Location)))
        (Format.asprintf "%s/create" base_path)
    in
    [ `column Pool_location.column_name
    ; `column Pool_location.column_description
    ; `custom
        (span
           Pool_common.
             [ Utils.text_to_string language I18n.Address
               |> CCString.capitalize_ascii
               |> txt
             ])
    ; `custom create_filter
    ]
  in
  let th_class = [ "w-3"; "w-4"; "w-4"; "w-1" ] in
  let row (location : Pool_location.t) =
    let open Pool_location in
    [ txt (Name.value location.name)
    ; descriptions_all_languages location
    ; Component.Partials.address_to_html language location.address
    ; location_specific_path location.id
      |> Component.Input.link_as_button ~icon:Component.Icon.Eye
    ]
    |> CCList.map (CCList.return %> td)
    |> tr
  in
  Component.DataTable.make
    ~align_top:true
    ~target_id:"location-table"
    ~th_class
    ~cols
    ~row
    data_table
    location_list
;;

let index (Pool_context.{ language; _ } as context) location_list query =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt Pool_common.(Utils.text_to_string language I18n.LocationListTitle)
        ]
    ; p
        Pool_common.
          [ Utils.hint_to_string language I18n.LocationsIndex
            |> HttpUtils.add_line_breaks
          ]
    ; list context location_list query
    ]
;;

let file_form
  (labels : Pool_location.Mapping.Label.t list)
  (languages : Pool_common.Language.t list)
  (location : Pool_location.t)
  Pool_context.{ language; csrf; _ }
  =
  let open Pool_location in
  let action = location_specific_path ~suffix:"files" location.id in
  let label_select =
    let open Mapping.Label in
    selector language Pool_message.Field.Label show labels None ()
  in
  let language_select =
    let open Pool_common.Language in
    selector language Pool_message.Field.Language show languages None ()
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "narrow"; "stack" ] ]
    [ h1
        [ [ Pool_common.(Utils.text_to_string language I18n.LocationFileNew)
          ; location.name |> Name.show
          ]
          |> CCString.concat " "
          |> txt
        ]
    ; form
        ~a:
          [ a_method `Post
          ; a_action (Sihl.Web.externalize_path action)
          ; a_enctype "multipart/form-data"
          ; a_class [ "stack" ]
          ; a_user_data "detect-unsaved-changes" ""
          ]
        [ csrf_element csrf ()
        ; label_select
        ; language_select
        ; input_element_file
            language
            ~allow_multiple:false
            ~required:true
            Pool_message.Field.FileMapping
        ; div
            ~a:[ a_class [ "flexrow" ] ]
            [ submit_element
                ~classnames:[ "push" ]
                language
                Message.(Control.Add (Some Field.File))
                ~submit_type:`Primary
                ()
            ]
        ]
    ]
;;

let form
  ?(location : Pool_location.t option)
  ?(states : Pool_location.Status.t list = [])
  Pool_context.{ language; csrf; _ }
  tenant_languages
  flash_fetcher
  =
  let open Pool_location in
  let default = "" in
  let action =
    location
    |> CCOption.map_or ~default:base_path (fun { id; _ } ->
      location_specific_path id)
  in
  let value field_fcn decode_fcn =
    let open CCOption.Infix in
    location >|= field_fcn |> CCOption.map_or ~default decode_fcn
  in
  let value_opt field_fcn decode_fcn =
    let open CCOption.Infix in
    location >>= field_fcn |> CCOption.map_or ~default decode_fcn
  in
  let status_select_opt =
    match location with
    | Some { status; _ } ->
      selector
        language
        Pool_message.Field.Status
        Status.show
        states
        (Some status)
        ~required:true
        ()
    | None -> txt ""
  in
  let address_value fcn =
    location
    |> CCOption.map_or ~default (fun ({ address; _ } : t) ->
      match address with
      | Address.Virtual -> default
      | Address.Physical m -> m |> fcn)
  in
  let is_virtual =
    location
    |> CCOption.map_or ~default:false (fun ({ address; _ } : t) ->
      match address with
      | Address.Virtual -> true
      | Address.Physical _ -> false)
  in
  let is_virtual_checkbox =
    let checked = if is_virtual then [ a_checked () ] else [] in
    input
      ~a:
        ([ a_id "toggle-address"
         ; a_input_type `Checkbox
         ; a_name Pool_message.Field.(Virtual |> show)
         ]
         @ checked)
      ()
  in
  let description_html =
    let field description_language =
      let open CCOption in
      let name = Description.field_name description_language in
      let current =
        location
        >>= fun { description; _ } ->
        description >>= Description.find_opt description_language
      in
      let value =
        flash_fetcher name <+> current |> CCOption.get_or ~default:""
      in
      div
        ~a:[ a_class [ "form_group" ] ]
        [ label
            ~a:[ a_label_for name ]
            [ txt (Pool_common.Language.show description_language) ]
        ; textarea
            ~a:[ a_id name; a_name name; a_class [ "rich-text" ] ]
            (txt value)
        ]
    in
    tenant_languages
    |> CCList.map field
    |> fun textareas ->
    div
      ~a:[ a_class [ "full-width" ] ]
      [ h3
          [ txt
              Pool_common.(
                Utils.field_to_string language Pool_message.Field.description
                |> CCString.capitalize_ascii)
          ]
      ; div ~a:[ a_class [ "grid-col-2" ] ] textareas
      ]
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        [ txt
            Pool_common.(Utils.text_to_string Language.En I18n.LocationNewTitle)
        ]
    ; form
        ~a:
          [ a_method `Post
          ; a_action (Sihl.Web.externalize_path action)
          ; a_class [ "stack" ]
          ; a_user_data "detect-unsaved-changes" ""
          ]
        ([ csrf_element csrf ()
         ; div
             ~a:[ a_class [ "grid-col-2" ] ]
             [ input_element
                 language
                 `Text
                 Pool_message.Field.Name
                 ~value:(value (fun m -> m.name) Name.value)
                 ~flash_fetcher
                 ~required:true
             ; input_element
                 language
                 `Text
                 Pool_message.Field.Link
                 ~value:(value_opt (fun m -> m.link) Link.value)
                 ~flash_fetcher
             ; description_html
             ; status_select_opt
             ]
         ]
         @ [ div
               [ h4
                   ~a:[ a_class [ "heading-4" ] ]
                   [ txt
                       (Pool_message.Field.Location
                        |> Pool_common.Utils.field_to_string language
                        |> CCString.capitalize_ascii)
                   ]
               ; div
                   ~a:[ a_class [ "stack" ] ]
                   [ is_virtual_checkbox
                   ; label
                       ~a:[ a_label_for "toggle-address" ]
                       [ txt
                           Pool_message.Field.(
                             Virtual |> show |> CCString.capitalize_ascii)
                       ]
                   ; div
                       ~a:
                         [ a_id "address-subform"
                         ; a_class [ "stack"; "flexcolumn" ]
                         ]
                       [ input_element
                           language
                           `Text
                           Pool_message.Field.Institution
                           ~flash_fetcher
                           ~value:
                             (address_value
                                Address.Mail.(
                                  fun { institution; _ } ->
                                    institution
                                    |> CCOption.map_or
                                         ~default:""
                                         Institution.value))
                       ; div
                           ~a:[ a_class [ "switcher"; "flex-gap" ] ]
                           [ input_element
                               language
                               `Text
                               Pool_message.Field.Room
                               ~flash_fetcher
                               ~value:
                                 (address_value
                                    Address.Mail.(
                                      fun { room; _ } ->
                                        CCOption.map_or ~default Room.value room))
                           ; input_element
                               language
                               `Text
                               Pool_message.Field.Building
                               ~flash_fetcher
                               ~value:
                                 (address_value
                                    Address.Mail.(
                                      fun { building; _ } ->
                                        building
                                        |> CCOption.map_or
                                             ~default:""
                                             Building.value))
                           ]
                       ; input_element
                           language
                           `Text
                           Pool_message.Field.Street
                           ~required:true
                           ~flash_fetcher
                           ~value:
                             Address.Mail.(
                               address_value (fun { street; _ } ->
                                 Street.value street))
                       ; div
                           ~a:[ a_class [ "switcher"; "flex-gap" ] ]
                           [ input_element
                               language
                               `Text
                               Pool_message.Field.Zip
                               ~required:true
                               ~flash_fetcher
                               ~value:
                                 Address.Mail.(
                                   address_value (fun { zip; _ } ->
                                     Zip.value zip))
                           ; input_element
                               language
                               `Text
                               Pool_message.Field.City
                               ~required:true
                               ~flash_fetcher
                               ~value:
                                 Address.Mail.(
                                   address_value (fun { city; _ } ->
                                     City.value city))
                           ]
                       ]
                   ]
               ]
           ; div
               ~a:[ a_class [ "flexrow" ] ]
               [ submit_element
                   ~classnames:[ "push" ]
                   language
                   Message.Control.(
                     let field = Some Field.location in
                     match location with
                     | None -> Create field
                     | Some _ -> Update field)
                   ~submit_type:`Primary
                   ()
               ]
           ])
    ; script
        (Unsafe.data
           {sql|
      const toggle = document.getElementById("toggle-address");
      const subform = document.getElementById("address-subform");
      const inputs = subform.querySelectorAll("input");

      const toggleInputs = (disabled) => {
        inputs.forEach( el => el.disabled = disabled)
      }

      const toggleActive = (checkbox) => {
        if(checkbox.checked) {
          subform.style.display = "none";
        } else {
          subform.style.display = "flex";
        }
        toggleInputs(checkbox.checked);
      }

      toggle.addEventListener("click", (e) => toggleActive(e.currentTarget));
      toggleActive(toggle);
    |sql})
    ]
;;

module FileList = struct
  open Pool_location

  let add_link location_id language =
    let open Pool_common in
    a
      ~a:
        [ location_specific_path ~suffix:"files/create" location_id
          |> Sihl.Web.externalize_path
          |> a_href
        ]
      Message.
        [ Control.Add (Some Field.File)
          |> Utils.control_to_string language
          |> txt
        ]
  ;;

  let add_file_btn language id =
    link_as_button
      ~style:`Success
      ~icon:Icon.Create
      ~classnames:[ "small" ]
      ~control:(language, Message.(Control.Add (Some Field.File)))
      (location_specific_path ~suffix:"files/create" id)
  ;;

  let thead language location_id =
    (Pool_message.Field.[ Label; Language ]
     |> Component.Table.fields_to_txt language)
    @ [ add_file_btn language location_id ]
  ;;

  let row
    page_language
    csrf
    location_id
    (Mapping.{ id; label; language; _ } as file)
    =
    let delete_form =
      Tyxml.Html.form
        ~a:
          [ a_method `Post
          ; a_action
              (Format.asprintf
                 "/admin/locations/%s/mapping/%s/delete"
                 (Id.value location_id)
                 (Mapping.Id.value id)
               |> Sihl.Web.externalize_path)
          ; a_user_data
              "confirmable"
              Pool_common.(
                Utils.confirmable_to_string page_language I18n.DeleteFile)
          ]
        [ csrf_element csrf ()
        ; submit_element
            page_language
            Message.(Control.Delete (Some Field.File))
            ~submit_type:`Error
            ()
        ]
    in
    [ label |> Mapping.Label.show |> txt
    ; language |> Pool_common.Language.show |> txt
    ; div
        ~a:[ a_class [ "flexrow"; "flex-gap"; "align-center" ] ]
        [ admin_file_path location_id file |> edit_link; delete_form ]
    ]
  ;;

  (* TODO: add link and message, if list is empty *)
  let create csrf language ({ id; files; _ } : Pool_location.t) =
    let form =
      match CCList.is_empty files with
      | true ->
        div
          [ p
              [ Pool_common.(
                  I18n.LocationNoFiles |> Utils.text_to_string language)
                |> txt
              ]
          ; div [ add_file_btn language id ]
          ]
      | false ->
        let body = CCList.map (row language csrf id) files in
        Component.Table.horizontal_table
          `Striped
          ~align_last_end:true
          ~thead:(thead language id)
          body
    in
    div
      [ h2
          ~a:[ a_class [ "heading-3" ] ]
          [ txt Pool_common.(Utils.text_to_string language I18n.Files) ]
      ; p
          Pool_common.
            [ Utils.hint_to_string language I18n.LocationFiles
              |> HttpUtils.add_line_breaks
            ]
      ; form
      ]
  ;;
end

module SessionList = struct
  let session_title (session : Session.Public.t) =
    session.Session.Public.start
    |> Session.Start.value
    |> Pool_model.Time.formatted_date_time
    |> Format.asprintf "Session at %s"
  ;;

  let rows sessions =
    CCList.map
      (fun (session, (experiment_id, experiment_title)) ->
        let open Session.Public in
        [ session |> session_title |> txt
        ; experiment_title |> txt
        ; session.duration
          |> Session.Duration.value
          |> Pool_model.Time.formatted_timespan
          |> txt
        ; session.canceled_at
          |> CCOption.map_or ~default:"" (fun t ->
            Pool_model.Time.formatted_date_time t)
          |> txt
        ; Format.asprintf
            "/admin/experiments/%s/sessions/%s"
            (Experiment.Id.value experiment_id)
            Session.(session.id |> Id.value)
          |> edit_link
        ])
      sessions
  ;;
end

let detail
  (location : Pool_location.t)
  statistics
  statistics_year_range
  Pool_context.{ csrf; language; _ }
  =
  let open Pool_location in
  let location_details =
    let open Pool_message in
    [ Field.Name, location.name |> Name.value |> txt
    ; Field.Description, descriptions_all_languages location
    ; ( Field.Location
      , Component.Partials.address_to_html language location.address )
    ; Field.Link, location.link |> CCOption.map_or ~default:"" Link.value |> txt
    ; Field.Status, location.status |> Status.show |> txt (* TODO: Show files *)
    ]
    |> Component.Table.vertical_table
         ~align_top:true
         ~break_mobile:true
         ~th_class:[ "w-4" ]
         `Striped
         language
  in
  let edit_button =
    link_as_button
      ~icon:Icon.Create
      ~classnames:[ "small" ]
      ~control:(language, Pool_message.(Control.Edit (Some Field.Location)))
      (location_specific_path ~suffix:"edit" location.Pool_location.id)
  in
  let public_page_link =
    p
      [ a
          ~a:
            [ a_href
                (Format.asprintf
                   "/location/%s"
                   (location.Pool_location.id |> Id.value)
                 |> Sihl.Web.externalize_path)
            ; a_target "_blank"
            ]
          [ txt
              Pool_common.(
                Utils.control_to_string language Message.Control.PublicPage
                |> CCString.capitalize_ascii)
          ]
      ]
  in
  div
    ~a:[ a_class [ "safety-margin"; "trim" ] ]
    [ div
        ~a:[ a_class [ "stack-lg" ] ]
        [ div
            ~a:[ a_class [ "stack" ] ]
            [ div
                [ div
                    ~a:
                      [ a_class [ "flexrow"; "justify-between"; "align-center" ]
                      ]
                    [ div
                        [ h1
                            ~a:[ a_class [ "heading-1" ] ]
                            [ txt (location.name |> Name.value) ]
                        ]
                    ; div [ edit_button ]
                    ]
                ; div
                    ~a:[ a_class [ "gap-lg" ] ]
                    [ div
                        ~a:[ a_class [ "grid-col-3" ] ]
                        [ div ~a:[ a_class [ "span-2" ] ] [ location_details ]
                        ; div
                            ~a:
                              [ a_class [ "inset"; "border"; "bg-grey-light" ] ]
                            [ make_statistics
                                statistics_year_range
                                language
                                location.id
                                statistics
                            ]
                        ]
                    ]
                ]
            ; public_page_link
            ; FileList.create csrf language location
            ]
        ; Component.Calendar.(create (Location location.Pool_location.id))
        ]
    ]
;;
