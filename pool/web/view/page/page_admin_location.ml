open Tyxml.Html
open Component
module Message = Pool_common.Message

let selector field equal show options selected ?(attributes = []) () =
  let name = Message.Field.(show field) in
  div
    [ label [ name |> CCString.capitalize_ascii |> txt ]
    ; div
        ~a:[ a_class [ "select" ] ]
        [ select
            ~a:(a_name name :: attributes)
            (CCList.map
               (fun l ->
                 let is_selected =
                   selected
                   |> CCOption.map_or ~default:[] (fun selected ->
                          if equal selected l then [ a_selected () ] else [])
                 in
                 option
                   ~a:((l |> show |> a_value) :: is_selected)
                   (l |> show |> CCString.capitalize_ascii |> txt))
               options)
        ]
    ]
;;

let first_n_characters ?(n = 47) m : string =
  if CCString.length m > n
  then CCString.sub m 0 n |> Format.asprintf "%s..."
  else m
;;

module List = struct
  open Pool_location

  let thead language =
    let open Pool_common in
    let open Message in
    CCList.map
      (fun field ->
        field
        |> Utils.field_to_string language
        |> CCString.capitalize_ascii
        |> txt
        |> CCList.pure)
      [ Field.Name; Field.Description; Field.Location ]
    @ [ Add (Some Field.Location)
        |> Utils.control_to_string language
        |> txt
        |> CCList.pure
        |> a ~a:[ a_href (Sihl.Web.externalize_path "/admin/locations/create") ]
        |> CCList.pure
      ]
    |> CCList.map th
    |> tr
    |> CCList.pure
    |> thead
  ;;

  let row language (location : Pool_location.t) =
    let address =
      let room, street, city =
        Address.address_rows_human language location.address
      in
      let mail =
        if street |> CCString.is_empty |> not
        then [ [ ""; street; city ] |> CCString.concat ", " |> txt ]
        else []
      in
      [ strong [ txt room ] ] @ mail
    in
    tr
      [ td [ location.name |> Name.value |> txt ]
      ; td
          [ location.description
            |> CCOption.map_or ~default:"" Description.value
            |> first_n_characters
            |> txt
          ]
      ; td address
      ; td
          [ p
              [ a
                  ~a:
                    [ a_href
                        (Sihl.Web.externalize_path
                           (Format.asprintf
                              "/admin/locations/%s"
                              (location.Pool_location.id |> Id.value)))
                    ]
                  [ txt
                      Pool_common.(
                        Message.More |> Utils.control_to_string language)
                  ]
              ]
          ]
      ]
  ;;

  let create ?(classnames = []) language locations =
    let thead = thead language in
    let body = CCList.map (row language) locations in
    table ~a:[ a_class classnames ] ~thead body
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
             (value (fun m -> m.name) Name.value)
         ; input_element
             language
             `Text
             Message.Field.Description
             (value_opt (fun m -> m.description) Description.value)
         ; input_element
             language
             `Text
             Message.Field.Link
             (value_opt (fun m -> m.link) Link.value)
         ]
        @ status_select_opt
        @ [ div
              [ label
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
                      ~a:[ a_class [ "toggled"; "switcher"; "flex-gap" ] ]
                      [ input_element
                          language
                          `Text
                          Message.Field.Room
                          (address_value
                             Address.Mail.(fun { room; _ } -> Room.value room))
                      ; input_element
                          language
                          `Text
                          Message.Field.Building
                          (address_value
                             Address.Mail.(
                               fun { building; _ } ->
                                 building
                                 |> CCOption.map_or ~default:"" Building.value))
                      ]
                  ; input_element
                      ~classnames:[ "toggled" ]
                      language
                      `Text
                      Message.Field.Street
                      Address.Mail.(
                        address_value (fun { street; _ } -> Street.value street))
                  ; div
                      ~a:[ a_class [ "toggled"; "switcher"; "flex-gap" ] ]
                      [ input_element
                          language
                          `Text
                          Message.Field.Zip
                          Address.Mail.(
                            address_value (fun { zip; _ } -> Zip.value zip))
                      ; input_element
                          language
                          `Text
                          Message.Field.City
                          Address.Mail.(
                            address_value (fun { city; _ } -> City.value city))
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

  let thead location_id language =
    let open Pool_common in
    let open Message in
    let open CCList in
    map
      (fun field ->
        [ field
          |> Utils.field_to_string language
          |> CCString.capitalize_ascii
          |> txt
        ])
      [ Field.Label; Field.Start ]
    @ [ add_link location_id language |> pure ]
    |> map th
    |> tr
    |> pure
    |> thead
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
          ]
        [ Component.csrf_element csrf ()
        ; submit_element
            language
            Message.(Delete (Some Field.FileMapping))
            ~submit_type:`Error
            ()
        ]
    in
    tr
      [ td [ label |> Mapping.Label.show |> txt ]
      ; td [ language |> Pool_common.Language.show |> txt ]
      ; td
          ~a:[ a_class [ "flexrow"; "flex-gap" ] ]
          [ p
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
      ]
  ;;

  let create
      csrf
      ?(classnames = [])
      language
      ({ id; files; _ } : Pool_location.t)
    =
    if CCList.is_empty files
    then add_link id language
    else (
      let thead = thead id language in
      let body = CCList.map (row csrf language id) files in
      table ~a:[ a_class classnames ] ~thead body)
  ;;
end

module SessionList = struct
  let session_title (session : Session.Public.t) =
    session.Session.Public.start
    |> Session.Start.value
    |> Pool_common.Utils.Time.formatted_date_time
    |> Format.asprintf "Session at %s"
  ;;

  let thead language =
    let open Pool_common in
    let open Message in
    let open CCList in
    map
      (fun field ->
        [ field
          |> Utils.field_to_string language
          |> CCString.capitalize_ascii
          |> txt
        ])
      [ Field.Session; Field.Experiment; Field.Start; Field.Duration ]
    @ [ [ txt "" ] ]
    |> map th
    |> tr
    |> pure
    |> thead
  ;;

  let row
      language
      ((session, (experiment_id, experiment_title)) :
        Session.Public.t * (Pool_common.Id.t * string))
    =
    let open Session.Public in
    tr
      [ td
          [ session
            |> session_title
            |> (Format.asprintf "%s %s"
               @@
               (* TODO [aerben] improve this *)
               if CCOption.is_some session.Session.Public.canceled_at
               then "CANCELED"
               else "")
            |> txt
          ]
      ; td [ experiment_title |> txt ]
      ; td
          [ session.start
            |> Session.Start.value
            |> Pool_common.Utils.Time.formatted_date_time
            |> txt
          ]
      ; td
          [ p
              [ a
                  ~a:
                    [ Format.asprintf
                        "/admin/experiments/%s/sessions/%s"
                        (Pool_common.Id.value experiment_id)
                        (Pool_common.Id.value session.id)
                      |> Sihl.Web.externalize_path
                      |> a_href
                    ]
                  [ txt
                      Pool_common.(
                        Message.More |> Utils.control_to_string language)
                  ]
              ]
          ]
      ]
  ;;

  let create ?(classnames = []) language sessions =
    if CCList.is_empty sessions
    then
      div
        [ Pool_common.(I18n.LocationNoSessions |> Utils.text_to_string language)
          |> txt
        ]
    else (
      let thead = thead language in
      let body = CCList.map (row language) sessions in
      table ~a:[ a_class classnames ] ~thead body)
  ;;
end

let detail
    (location : Pool_location.t)
    Pool_context.{ csrf; language; _ }
    sessions
  =
  let open Pool_location in
  div
    ~a:[ a_class [ "safety-margin"; "trim"; "measure" ] ]
    [ h1 ~a:[ a_class [ "heading-1" ] ] [ txt (location.name |> Name.value) ]
    ; p
        [ txt
            (location.description
            |> CCOption.map_or ~default:"" Description.value)
        ]
    ; FileList.create csrf language location
    ; SessionList.create language sessions
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
;;
