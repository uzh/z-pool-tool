open Tyxml.Html
open Component
module Message = Pool_common.Message

let first_n_characters ?(n = 47) m : string =
  if CCString.length m > n
  then CCString.sub m 0 n |> Format.asprintf "%s..."
  else m
;;

module List = struct
  open Pool_location

  let thead =
    Pool_common.Message.Field.
      [ Some Name; Some Description; Some Location; None ]
  ;;

  let rows language locations =
    CCList.map
      (fun (location : Pool_location.t) ->
        [ location.name |> Name.value |> txt
        ; location.description
          |> CCOption.map_or ~default:"" Description.value
          |> first_n_characters
          |> txt
        ; Pool_location.Address.to_html language location.address
        ; a
            ~a:
              [ a_href
                  (Sihl.Web.externalize_path
                     (Format.asprintf
                        "/admin/locations/%s"
                        (location.Pool_location.id |> Id.value)))
              ]
            [ txt Pool_common.(Message.More |> Utils.control_to_string language)
            ]
        ])
      locations
  ;;

  let create language locations =
    let rows = rows language locations in
    Table.horizontal_table `Striped language ~thead ~align_top:true rows
  ;;
end

let index location_list Pool_context.{ language; _ } =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt Pool_common.(Utils.text_to_string language I18n.LocationListTitle)
        ]
    ; List.create language location_list
    ; p
        [ a
            ~a:[ a_href (Sihl.Web.externalize_path "/admin/locations/create") ]
            [ txt
                Pool_common.(
                  Message.(Create (Some Field.Location))
                  |> Utils.control_to_string language)
            ]
        ]
    ]
;;

let file_form
    (labels : Pool_location.Mapping.Label.t list)
    (languages : Pool_common.Language.t list)
    (location : Pool_location.t)
    Pool_context.{ language; csrf; _ }
  =
  let open Pool_location in
  let action =
    location.id |> Id.value |> Format.asprintf "/admin/locations/%s/files"
  in
  let label_select =
    let open Mapping.Label in
    selector Message.Field.Label equal show labels None ()
  in
  let language_select =
    let open Pool_common.Language in
    selector Message.Field.Language equal show languages None ()
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
          ]
        [ Component.csrf_element csrf ()
        ; label_select
        ; language_select
        ; input_element_file
            language
            ~allow_multiple:false
            ~required:true
            Message.Field.FileMapping
        ; submit_element
            language
            Message.(Create (Some Field.FileMapping))
            ~submit_type:`Success
            ()
        ]
    ]
;;

let form
    ?(location : Pool_location.t option)
    ?(states : Pool_location.Status.t list = [])
    Pool_context.{ language; csrf; _ }
    flash_fetcher
  =
  let open Pool_location in
  let path = "/admin/locations" in
  let default = "" in
  let action =
    location
    |> CCOption.map_or ~default:path (fun { id; _ } ->
           id |> Id.value |> Format.asprintf "%s/%s" path)
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
      [ selector
          Message.Field.Status
          Status.equal
          Status.show
          states
          (Some status)
          ()
      ]
    | None -> []
  in
  let address_value fcn =
    location
    |> CCOption.map_or ~default (fun ({ address; _ } : t) ->
           match address with
           | Address.Virtual -> default
           | Address.Physical m -> m |> fcn)
  in
  let is_virtual_checkbox =
    let selected =
      location
      |> CCOption.map_or ~default:[] (fun ({ address; _ } : t) ->
             match address with
             | Address.Virtual -> [ a_checked () ]
             | Address.Physical _ -> [])
    in
    input
      ~a:
        ([ a_class [ "toggle-address" ]
         ; a_input_type `Checkbox
         ; a_name Message.Field.(Virtual |> show)
         ]
        @ selected)
      ()
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "narrow"; "stack" ] ]
    [ h1
        [ txt
            Pool_common.(Utils.text_to_string Language.En I18n.LocationNewTitle)
        ]
    ; form
        ~a:
          [ a_method `Post
          ; a_action (Sihl.Web.externalize_path action)
          ; a_class [ "stack" ]
          ]
        ([ Component.csrf_element csrf ()
         ; input_element
             language
             `Text
             Message.Field.Name
             ~value:(value (fun m -> m.name) Name.value)
             ~flash_fetcher
         ; input_element
             language
             `Text
             Message.Field.Description
             ~value:(value_opt (fun m -> m.description) Description.value)
             ~flash_fetcher
         ; input_element
             language
             `Text
             Message.Field.Link
             ~value:(value_opt (fun m -> m.link) Link.value)
             ~flash_fetcher
         ]
        @ status_select_opt
        @ [ div
              [ h4
                  ~a:[ a_class [ "heading-4" ] ]
                  [ txt
                      (Message.Field.Location
                      |> Pool_common.Utils.field_to_string language
                      |> CCString.capitalize_ascii)
                  ]
              ; div
                  ~a:[ a_class [ "stack" ] ]
                  [ is_virtual_checkbox
                  ; label
                      [ txt
                          Message.Field.(
                            Virtual |> show |> CCString.capitalize_ascii)
                      ]
                  ; div
                      ~a:[ a_class [ "toggled"; "stack"; "flexcolumn" ] ]
                      [ input_element
                          language
                          `Text
                          Message.Field.Institution
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
                              Message.Field.Room
                              ~value:
                                (address_value
                                   Address.Mail.(
                                     fun { room; _ } -> Room.value room))
                          ; input_element
                              language
                              `Text
                              Message.Field.Building
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
                          Message.Field.Street
                          ~value:
                            Address.Mail.(
                              address_value (fun { street; _ } ->
                                  Street.value street))
                      ; div
                          ~a:[ a_class [ "switcher"; "flex-gap" ] ]
                          [ input_element
                              language
                              `Text
                              Message.Field.Zip
                              ~value:
                                Address.Mail.(
                                  address_value (fun { zip; _ } ->
                                      Zip.value zip))
                          ; input_element
                              language
                              `Text
                              Message.Field.City
                              ~value:
                                Address.Mail.(
                                  address_value (fun { city; _ } ->
                                      City.value city))
                          ]
                      ]
                  ]
              ]
          ; submit_element
              language
              Message.(
                let field = Some Field.location in
                match location with
                | None -> Create field
                | Some _ -> Update field)
              ~submit_type:`Success
              ()
          ])
    ]
;;

module FileList = struct
  open Pool_location

  let add_link location_id language =
    let open Pool_common in
    a
      ~a:
        [ location_id
          |> Pool_location.Id.value
          |> Format.asprintf "/admin/locations/%s/files/create"
          |> Sihl.Web.externalize_path
          |> a_href
        ]
      Message.
        [ Add (Some Field.File) |> Utils.control_to_string language |> txt ]
  ;;

  let thead =
    Pool_common.Message.Field.[ Some Label; Some Language; None; None ]
  ;;

  let row
      csrf
      visual_language
      location_id
      (Mapping.{ id; label; language; file } : Mapping.file)
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
              Pool_common.(Utils.confirmable_to_string language I18n.DeleteFile)
          ]
        [ Component.csrf_element csrf ()
        ; submit_element
            language
            Message.(Delete (Some Field.File))
            ~submit_type:`Error
            ()
        ]
    in
    [ label |> Mapping.Label.show |> txt
    ; language |> Pool_common.Language.show |> txt
    ; p
        [ a
            ~a:
              [ Format.asprintf
                  "/admin/locations/%s/files/%s"
                  (Id.value location_id)
                  Pool_common.(Id.value file.File.id)
                |> Sihl.Web.externalize_path
                |> a_href
              ]
            [ txt
                Pool_common.(
                  Message.More |> Utils.control_to_string visual_language)
            ]
        ]
    ; delete_form
    ]
  ;;

  (* TODO: add link and message, if list is empty *)
  let create csrf language ({ id; files; _ } : Pool_location.t) =
    let form =
      match CCList.is_empty files with
      | true ->
        div
          [ Pool_common.(I18n.LocationNoFiles |> Utils.text_to_string language)
            |> txt
          ]
      | false ->
        let body = CCList.map (row csrf language id) files in
        Table.horizontal_table `Striped language ~thead body
    in
    div
      [ h2
          ~a:[ a_class [ "heading-2" ] ]
          [ txt Pool_common.(Utils.text_to_string language I18n.Files) ]
      ; div ~a:[ a_class [ "stack" ] ] [ form; p [ add_link id language ] ]
      ]
  ;;
end

module SessionList = struct
  let session_title (session : Session.Public.t) =
    session.Session.Public.start
    |> Session.Start.value
    |> Pool_common.Utils.Time.formatted_date_time
    |> Format.asprintf "Session at %s"
  ;;

  let rows language sessions =
    CCList.map
      (fun (session, (experiment_id, experiment_title)) ->
        let open Session.Public in
        [ session |> session_title |> txt
        ; experiment_title |> txt
        ; session.duration
          |> Session.Duration.value
          |> Pool_common.Utils.Time.formatted_timespan
          |> txt
        ; session.canceled_at
          |> CCOption.map_or ~default:"" (fun t ->
                 Pool_common.Utils.Time.formatted_date_time t)
          |> txt
        ; a
            ~a:
              [ Format.asprintf
                  "/admin/experiments/%s/sessions/%s"
                  (Pool_common.Id.value experiment_id)
                  (Pool_common.Id.value session.id)
                |> Sihl.Web.externalize_path
                |> a_href
              ]
            [ txt Pool_common.(Message.More |> Utils.control_to_string language)
            ]
        ])
      sessions
  ;;

  let create language sessions =
    let html =
      if CCList.is_empty sessions
      then
        div
          [ Pool_common.(
              I18n.LocationNoSessions |> Utils.text_to_string language)
            |> txt
          ]
      else (
        let thead =
          Pool_common.Message.Field.
            [ Some Session
            ; Some Experiment
            ; Some Duration
            ; Some CanceledAt
            ; None
            ]
        in
        let rows = rows language sessions in
        Table.horizontal_table `Striped language ~thead rows)
    in
    div
      [ h2
          ~a:[ a_class [ "heading-2" ] ]
          [ txt Pool_common.(Utils.nav_link_to_string language I18n.Sessions) ]
      ; div ~a:[ a_class [ "stack" ] ] [ html ]
      ]
  ;;
end

let detail
    (location : Pool_location.t)
    Pool_context.{ csrf; language; _ }
    sessions
  =
  let open Pool_location in
  let location_details =
    let open Pool_common.Message in
    let table =
      [ Field.Name, location.name |> Name.value |> txt
      ; ( Field.Description
        , location.description
          |> CCOption.map_or ~default:"" Description.value
          |> txt )
      ; Field.Location, Address.to_html language location.address
      ; ( Field.Link
        , location.link |> CCOption.map_or ~default:"" Link.value |> txt )
      ; ( Field.Status
        , location.status |> Status.show |> txt (* TODO: Show files *) )
      ]
      |> Table.vertical_table `Striped ~align_top:true language
    in
    div
      ~a:[ a_class [ "stack" ] ]
      [ table
      ; p
          [ a
              ~a:
                [ a_href
                    (Sihl.Web.externalize_path
                       (Format.asprintf
                          "/admin/locations/%s/edit"
                          (location.Pool_location.id |> Id.value)))
                ]
              [ txt
                  Pool_common.(
                    Message.(Edit (Some Field.Location))
                    |> Utils.control_to_string language)
              ]
          ]
      ]
  in
  div
    ~a:[ a_class [ "safety-margin"; "trim" ] ]
    [ h1 ~a:[ a_class [ "heading-1" ] ] [ txt (location.name |> Name.value) ]
    ; div
        ~a:[ a_class [ "stack-lg" ] ]
        [ location_details
        ; FileList.create csrf language location
        ; SessionList.create language sessions
        ]
    ]
;;
