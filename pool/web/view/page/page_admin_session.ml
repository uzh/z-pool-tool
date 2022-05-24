open Tyxml.Html
open Component
module Message = Pool_common.Message

let session_title (session : Session.t) =
  session.Session.start
  |> Session.Start.value
  |> Pool_common.Utils.Time.formatted_date_time
  |> Format.asprintf "Session at %s"
;;

let create csrf language experiment_id flash_fetcher =
  div
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt Pool_common.(Utils.text_to_string language I18n.SessionNewTitle) ]
    ; form
        ~a:
          [ a_class [ "stack" ]
          ; a_method `Post
          ; a_action
              (Format.asprintf "/admin/experiments/%s/sessions"
               @@ Pool_common.Id.value experiment_id
              |> Sihl.Web.externalize_path)
          ]
        [ Component.csrf_element csrf ()
        ; input_element_persistent
            language
            `Datetime
            Pool_common.Message.Field.Start
            flash_fetcher
        ; input_element_persistent
            language
            (* TODO [aerben] make this `Time and convert span from flatpickr to
               seconds *)
            `Number
            Pool_common.Message.Field.Duration
            ~info:"in seconds"
            flash_fetcher
          (* TODO [aerben] this should be textarea *)
        ; input_element_persistent
            language
            `Text
            Pool_common.Message.Field.Description
            flash_fetcher
        ; input_element_persistent
            language
            `Number
            Pool_common.Message.Field.MaxParticipants
            flash_fetcher
        ; input_element_persistent
            language
            `Number
            Pool_common.Message.Field.MinParticipants
            flash_fetcher
            ~default:"0"
        ; input_element_persistent
            language
            `Number
            Pool_common.Message.Field.Overbook
            flash_fetcher
        ; submit_element language Message.(Create (Some Field.Session)) ()
        ]
    ]
;;

let index
    Pool_context.{ language; csrf; _ }
    experiment_id
    sessions
    flash_fetcher
  =
  let session_row (session : Session.t) =
    let open Session in
    div
      ~a:[ a_class [ "inset-sm"; "flexrow"; "space-between"; "align-center" ] ]
      [ span
          [ txt
              (Format.asprintf "%s %s" (session |> session_title)
              @@
              (* TODO [aerben] improve this *)
              if CCOption.is_some session.Session.canceled_at
              then "CANCELED"
              else "")
          ]
      ; a
          ~a:
            [ a_href
                (Format.asprintf
                   "/admin/experiments/%s/sessions/%s"
                   (Pool_common.Id.value experiment_id)
                   (Pool_common.Id.value session.id)
                |> Sihl.Web.externalize_path)
            ]
          (* TODO [aerben] use Message.More *)
          [ txt "More" ]
      ; form
          ~a:
            [ a_method `Post
            ; a_action
                (Format.asprintf
                   "/admin/experiments/%s/sessions/%s/cancel"
                   (Pool_common.Id.value experiment_id)
                   (Pool_common.Id.value session.id)
                |> Sihl.Web.externalize_path)
            ]
          [ Component.csrf_element csrf ()
          ; submit_element language Message.(Cancel (Some Field.Session)) ()
          ]
      ; form
          ~a:
            [ a_method `Post
            ; a_action
                (Format.asprintf
                   "/admin/experiments/%s/sessions/%s/delete"
                   (Pool_common.Id.value experiment_id)
                   (Pool_common.Id.value session.id)
                |> Sihl.Web.externalize_path)
            ]
          [ Component.csrf_element csrf ()
          ; submit_element
              language
              Message.(Delete (Some Field.Session))
              ~submit_type:`Error
              ()
          ]
      ]
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ div
        ~a:[ a_class [ "stack-lg" ] ]
        [ Page_admin_experiments.subnav language experiment_id
        ; create csrf language experiment_id flash_fetcher
        ; div
            [ h2
                ~a:[ a_class [ "heading-2" ] ]
                [ txt
                    Pool_common.(
                      Utils.text_to_string language I18n.SessionListTitle)
                ]
            ; div ~a:[ a_class [ "striped" ] ] (CCList.map session_row sessions)
            ]
        ]
    ]
;;

let detail Pool_context.{ language; _ } experiment_id (session : Session.t) _ =
  let open Session in
  div
    ~a:[ a_class [ "trim"; "narrow"; "safety-margin" ] ]
    [ h1 ~a:[ a_class [ "heading-1" ] ] [ session |> session_title |> txt ]
    ; div
        ~a:[ a_class [ "stack" ] ]
        [ (* TODO [aerben] use better formatted date *)
          (let rows =
             let amount amt = amt |> ParticipantAmount.value |> string_of_int in
             let open Message in
             [ ( Field.Start
               , session.start |> Start.value |> Ptime.to_rfc3339 ~space:true )
             ; ( Field.Duration
               , session.duration
                 |> Duration.value
                 |> Pool_common.Utils.print_time_span )
             ; ( Field.Description
               , CCOption.map_or
                   ~default:""
                   Description.value
                   session.description )
             ; ( Field.Location
               , CCString.concat
                   ", "
                   (Pool_location.Address.address_rows_human
                      language
                      session.location.Pool_location.address
                   |> fun (room, street, city) ->
                   [ room; street; city ]
                   |> CCList.filter (fun m -> m |> CCString.is_empty |> not)) )
             ; Field.MaxParticipants, amount session.max_participants
             ; Field.MinParticipants, amount session.min_participants
             ; Field.Overbook, amount session.overbook
             ; ( Field.CanceledAt
               , CCOption.map_or
                   ~default:"Not canceled"
                   (Ptime.to_rfc3339 ~space:true)
                   session.canceled_at )
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
                         "/admin/experiments/%s/sessions/%s/edit"
                         (Pool_common.Id.value experiment_id)
                         (Pool_common.Id.value session.id)
                      |> Sihl.Web.externalize_path)
                  ]
                (* TODO [aerben] use Message.More *)
                [ Message.(Edit (Some Field.Session))
                  |> Pool_common.Utils.control_to_string language
                  |> txt
                ]
            ]
        ]
    ]
;;

let location_select options selected ?(attributes = []) () =
  let open Pool_location in
  let name = Message.Field.(show Location) in
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
                          if Pool_location.equal selected l
                          then [ a_selected () ]
                          else [])
                   |> CCOption.value ~default:[]
                 in
                 option
                   ~a:
                     ([ a_value (l.id |> Pool_location.Id.value) ] @ is_selected)
                   (txt (l.name |> Pool_location.Name.value)))
               options)
        ]
    ]
;;

let edit
    Pool_context.{ language; csrf; _ }
    experiment_id
    (session : Session.t)
    locations
  =
  let open Session in
  div
    ~a:[ a_class [ "trim"; "narrow"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt
            Pool_common.(Utils.text_to_string language I18n.SessionUpdateTitle)
        ]
    ; p [ session |> session_title |> txt ]
    ; (let amount amt = amt |> ParticipantAmount.value |> string_of_int in
       form
         ~a:
           [ a_class [ "stack" ]
           ; a_method `Post
           ; a_action
               (Format.asprintf
                  "/admin/experiments/%s/sessions/%s"
                  (Pool_common.Id.value experiment_id)
                  (Pool_common.Id.value session.id)
               |> Sihl.Web.externalize_path)
           ]
         [ Component.csrf_element csrf ()
           (* TODO [aerben] use better formatted date *)
         ; input_element
             language
             `Datetime
             Pool_common.Message.Field.Start
             (session.start |> Start.value |> Ptime.to_rfc3339 ~space:true)
         ; input_element
             language
             `Number
             Pool_common.Message.Field.Duration
             ~info:"in seconds"
             (session.duration
             |> Duration.value
             |> Pool_common.Utils.print_time_span)
           (* TODO [aerben] this should be textarea *)
         ; input_element language `Text Pool_common.Message.Field.Description
           @@ CCOption.map_or ~default:"" Description.value session.description
         ; location_select locations (Some session.location) ()
         ; input_element
             language
             `Number
             Pool_common.Message.Field.MaxParticipants
             (amount session.max_participants)
         ; input_element
             language
             `Number
             Pool_common.Message.Field.MinParticipants
             (amount session.min_participants)
         ; input_element
             language
             `Number
             Pool_common.Message.Field.Overbook
             (amount session.overbook)
         ; submit_element
             language
             Message.(Update (Some Field.Session))
             ~submit_type:`Success
             ()
         ])
    ]
;;
