open Tyxml.Html
open Component
module Message = Pool_common.Message

let status_select options selected ?(attributes = []) () =
  let open Pool_location in
  let name = Message.Field.(show Status) in
  div
    [ label [ txt (name |> CCString.capitalize_ascii) ]
    ; div
        ~a:[ a_class [ "select" ] ]
        [ select
            ~a:([ a_name name ] @ attributes)
            (CCList.map
               (fun l ->
                 let is_selected =
                   selected
                   |> CCOption.map (fun selected ->
                          if Status.equal selected l
                          then [ a_selected () ]
                          else [])
                   |> CCOption.value ~default:[]
                 in
                 option
                   ~a:([ a_value (Status.show l) ] @ is_selected)
                   (txt (l |> Status.show |> CCString.capitalize_ascii)))
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
        |> a ~a:[ a_href (Sihl.Web.externalize_path "/admin/locations/new") ]
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

let form
    ?(location : Pool_location.t option)
    ?(states : Pool_location.Status.t list = [])
    Pool_context.{ language; csrf; _ }
  =
  let open Pool_location in
  let action =
    location
    |> CCOption.map_or ~default:"/admin/locations" (fun m ->
           m.id |> Id.value |> Format.asprintf "/admin/locations/%s")
  in
  let value = CCFun.flip (CCOption.map_or ~default:"") location in
  let address_value ?(default = "") fcn =
    location
    |> (CCOption.map_or ~default) (fun (location : t) ->
           match location.address with
           | Address.Virtual -> default
           | Address.Address m -> m |> fcn)
  in
  let selected =
    location
    |> CCOption.map_or ~default:[] (fun (m : Pool_location.t) ->
           match m.address with
           | Address.Virtual -> [ a_checked () ]
           | Address.Address _ -> [])
  in
  let attrs =
    [ a_class [ "toggle-address" ]
    ; a_input_type `Checkbox
    ; a_name Message.Field.(Virtual |> show)
    ]
  in
  let checkbox = input ~a:(attrs @ selected) () in
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
        [ Component.csrf_element csrf ()
        ; input_element
            language
            `Text
            Message.Field.Name
            (value (fun m -> m.name |> Name.value))
        ; input_element
            language
            `Text
            Message.Field.Description
            (value (fun m ->
                 m.description |> CCOption.map_or ~default:"" Description.value))
        ; input_element
            language
            `Text
            Message.Field.Link
            (value (fun m -> m.link |> CCOption.map_or ~default:"" Link.value))
        ; status_select
            states
            (location |> CCOption.map (fun (m : Pool_location.t) -> m.status))
            ()
        ; div
            [ label
                [ txt
                    (Message.Field.Location
                    |> Pool_common.Utils.field_to_string language
                    |> CCString.capitalize_ascii)
                ]
            ; div
                [ checkbox
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
                        (address_value (fun m ->
                             Address.Mail.(m.room |> Room.value)))
                    ; input_element
                        language
                        `Text
                        Message.Field.Building
                        (address_value (fun m ->
                             Address.Mail.(
                               m.building
                               |> CCOption.map_or ~default:"" Building.value)))
                    ]
                ; input_element
                    ~classnames:[ "toggled" ]
                    language
                    `Text
                    Message.Field.Street
                    (address_value (fun m ->
                         Address.Mail.(m.street |> Street.value)))
                ; div
                    ~a:[ a_class [ "toggled"; "switcher"; "flex-gap" ] ]
                    [ input_element
                        language
                        `Text
                        Message.Field.Zip
                        (address_value (fun m ->
                             Address.Mail.(m.zip |> Zip.value)))
                    ; input_element
                        language
                        `Text
                        Message.Field.City
                        (address_value (fun m ->
                             Address.Mail.(m.city |> City.value)))
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
        ]
    ]
;;

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
    CCList.map
      (fun field ->
        field
        |> Utils.field_to_string language
        |> CCString.capitalize_ascii
        |> txt
        |> CCList.pure)
      [ Field.Session; Field.Experiment; Field.Start; Field.Duration ]
    @ [ [ txt "" ] ]
    |> CCList.map th
    |> tr
    |> CCList.pure
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
    let thead = thead language in
    let body = CCList.map (row language) sessions in
    table ~a:[ a_class classnames ] ~thead body
  ;;
end

let detail (location : Pool_location.t) Pool_context.{ language; _ } sessions =
  let open Pool_location in
  div
    ~a:[ a_class [ "safety-margin"; "trim"; "measure" ] ]
    [ h1 ~a:[ a_class [ "heading-1" ] ] [ txt (location.name |> Name.value) ]
    ; p
        [ txt
            (location.description
            |> CCOption.map_or ~default:"" Description.value)
        ]
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
