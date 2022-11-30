open Tyxml.Html

module DummyData = struct
  let location =
    let open Pool_location in
    let open CCResult in
    let get_exn = CCResult.get_exn in
    let address =
      let open Address in
      let mail =
        let open Mail in
        let* institution = "USZ" |> Institution.create in
        let* room = "SNS Lab" |> Room.create in
        let* building = "University Hospital Zurich" |> Building.create in
        let* street = "Rämistrasse 100" |> Street.create in
        let* zip = "8091" |> Zip.create in
        let* city = "Zurich" |> City.create in
        Ok
          { institution = Some institution
          ; room
          ; building = Some building
          ; street
          ; zip
          ; city
          }
      in
      let mail = mail |> get_exn in
      Physical mail
    in
    let name = "SNS Lab" |> Name.create |> get_exn in
    let address = address in
    let link =
      "https://www.zne.uzh.ch/en/facilities.html" |> Link.create |> get_exn
    in
    let status = Status.Active in
    let files = [] in
    { id = Id.create ()
    ; name
    ; description = None
    ; address
    ; link = Some link
    ; status
    ; files
    ; created_at = Ptime_clock.now ()
    ; updated_at = Ptime_clock.now ()
    }
  ;;

  let session =
    let hour = Ptime.Span.of_int_s @@ (60 * 60) in
    Session.
      { id = Pool_common.Id.create ()
      ; follow_up_to = None
      ; start =
          Ptime.add_span (Ptime_clock.now ()) hour
          |> CCOption.get_exn_or "Invalid start"
          |> Start.create
      ; duration = Duration.create hour |> Pool_common.Utils.get_or_failwith
      ; description = None
      ; location
      ; max_participants =
          ParticipantAmount.create 30 |> Pool_common.Utils.get_or_failwith
      ; min_participants =
          ParticipantAmount.create 1 |> Pool_common.Utils.get_or_failwith
      ; overbook =
          ParticipantAmount.create 4 |> Pool_common.Utils.get_or_failwith
      ; reminder_subject = None
      ; reminder_lead_time = None
      ; reminder_text = None
      ; reminder_sent_at = None
      ; assignment_count =
          0 |> AssignmentCount.create |> Pool_common.Utils.get_or_failwith
      ; closed_at = None
      ; canceled_at = None
      ; created_at = Pool_common.CreatedAt.create ()
      ; updated_at = Pool_common.UpdatedAt.create ()
      }
  ;;

  let experiment =
    let get_exn = CCResult.get_exn in
    Experiment.
      { id = Pool_common.Id.create ()
      ; title = Title.create "The Wallet Game\t" |> get_exn
      ; public_title = PublicTitle.create "public_title" |> get_exn
      ; description = Description.create "A description for everyone" |> get_exn
      ; filter = None
      ; invitation_template = None
      ; session_reminder_subject = None
      ; session_reminder_text = None
      ; session_reminder_lead_time = None
      ; direct_registration_disabled =
          false |> DirectRegistrationDisabled.create
      ; registration_disabled = false |> RegistrationDisabled.create
      ; allow_uninvited_signup = false |> AllowUninvitedSignup.create
      ; experiment_type = Some Pool_common.ExperimentType.Lab
      ; created_at = Ptime_clock.now ()
      ; updated_at = Ptime_clock.now ()
      }
  ;;

  let name_element = "name", div [ txt "John Doe" ]
end

let build_help language toggle_id help =
  let wrap_hints html =
    div
      ~a:[ a_class [ "flexcolumn" ] ]
      [ label
          ~a:[ a_label_for toggle_id; a_class [ "flexrow"; "flex-gap-xs" ] ]
          [ strong
              [ txt
                  Pool_common.(
                    Utils.text_to_string language I18n.TextTemplates
                    |> CCString.capitalize_ascii)
              ]
          ; Component_icon.icon `HelpOutline
          ]
      ; input
          ~a:[ a_input_type `Checkbox; a_class [ "toggle" ]; a_id toggle_id ]
          ()
      ; div
          ~a:
            [ a_class
                [ "gap"; "toggle-body"; "inset-sm"; "border"; "bg-grey-light" ]
            ]
          [ p
              [ txt
                  Pool_common.(
                    Utils.hint_to_string language I18n.TemplateTextElementsHint)
              ]
          ; html
          ]
      ]
  in
  help
  |> CCList.map (fun (elm, example) ->
       [ txt (Format.asprintf "{%s}" elm); example ])
  |> Component_table.horizontal_table `Simple language ~align_top:true
  |> wrap_hints
;;

let session_reminder_help language sys_languages ?session () =
  let session = CCOption.value ~default:DummyData.session session in
  let session_overview =
    (CCList.map (fun lang ->
       ( Format.asprintf "sessionOverview%s" (Pool_common.Language.show lang)
       , Session.(to_email_text lang session) |> Http_utils.add_line_breaks )))
      sys_languages
  in
  DummyData.name_element :: session_overview
  |> build_help language "session-reminder-help"
;;

let experiment_invitation_help language ?experiment () =
  let experiment = CCOption.value ~default:DummyData.experiment experiment in
  let experiment_description =
    ( "experimentDescription"
    , div [ txt Experiment.(Description.value experiment.description) ] )
  in
  [ DummyData.name_element; experiment_description ]
  |> build_help language "experiment-invitation-help"
;;
