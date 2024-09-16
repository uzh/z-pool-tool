module Conformist = Pool_conformist
open CCFun.Infix

let src = Logs.Src.create "assignment.cqrs"

type update =
  { no_show : Assignment.NoShow.t
  ; participated : Assignment.Participated.t
  ; external_data_id : Assignment.ExternalDataId.t option
  }

let update_command no_show participated external_data_id =
  { no_show; participated; external_data_id }
;;

let update_schema =
  let open Assignment in
  Conformist.(
    make
      Field.
        [ NoShow.schema ()
        ; Participated.schema ()
        ; Conformist.optional @@ ExternalDataId.schema ()
        ]
      update_command)
;;

let decode_update data =
  Conformist.decode_and_validate update_schema data
  |> CCResult.map_err Pool_message.to_conformist_error
;;

let assignment_effect action uuid =
  let target_id = uuid |> Guard.Uuid.target_of Assignment.Id.value in
  Guard.ValidationSet.one_of_tuple (action, `Assignment, Some target_id)
;;

let assignment_creation_and_confirmation_events
  confirmation_email
  session
  follow_up_sessions
  contact
  =
  let open CCResult in
  let open Assignment in
  let all_sessions = session :: follow_up_sessions in
  let* (_ : unit list) =
    all_sessions |> CCList.map Session.assignment_creatable |> CCList.all_ok
  in
  let follow_up_events =
    follow_up_sessions
    |> CCList.map (fun session -> Created (create contact, session.Session.id))
  in
  let main_assignment = create contact in
  let confirmation_email = confirmation_email main_assignment in
  let email_event = Email.sent confirmation_email |> Pool_event.email in
  let create_events =
    Created (main_assignment, session.Session.id) :: follow_up_events
    |> CCList.map Pool_event.assignment
  in
  Ok (email_event :: create_events)
;;

module Create : sig
  include Common.CommandSig

  type t =
    { contact : Contact.t
    ; session : Session.t
    ; follow_up_sessions : Session.t list
    ; experiment : Experiment.t
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> ?direct_enrollment_by_admin:bool
    -> t
    -> (Assignment.t -> Email.dispatch)
    -> bool
    -> (Pool_event.t list, Pool_message.Error.t) result

  val effects : Experiment.Id.t -> Guard.ValidationSet.t
end = struct
  type t =
    { contact : Contact.t
    ; session : Session.t
    ; follow_up_sessions : Session.t list
    ; experiment : Experiment.t
    }

  let handle
    ?(tags = Logs.Tag.empty)
    ?(direct_enrollment_by_admin = false)
    { contact; session; follow_up_sessions; experiment }
    confirmation_email
    already_enrolled
    =
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
    let open CCResult in
    let open Pool_message.Error in
    let all_sessions = session :: follow_up_sessions in
    let* () =
      if Contact.is_inactive contact then Error ContactIsInactive else Ok ()
    in
    if already_enrolled
    then Error AlreadySignedUpForExperiment
    else
      let* () =
        let open Experiment in
        if direct_enrollment_by_admin
        then Ok ()
        else
          (experiment.direct_registration_disabled
           |> DirectRegistrationDisabled.value
           || experiment.registration_disabled |> RegistrationDisabled.value)
          |> Utils.bool_to_result_not DirectRegistrationIsDisabled
      in
      let* creation_events =
        assignment_creation_and_confirmation_events
          confirmation_email
          session
          follow_up_sessions
          contact
      in
      let contact_event =
        Contact_counter.update_on_session_signup contact all_sessions
        |> Contact.updated
        |> Pool_event.contact
      in
      Ok (creation_events @ [ contact_event ])
  ;;

  let effects = Assignment.Guard.Access.create
end

module Cancel : sig
  include Common.CommandSig with type t = Assignment.t list * Session.t

  val handle
    :  ?tags:Logs.Tag.set
    -> Admin.t
    -> Email.dispatch
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val effects : Experiment.Id.t -> Assignment.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Assignment.t list * Session.t

  let handle
    ?(tags = Logs.Tag.empty)
    admin
    notification_email
    (assignments, session)
    : (Pool_event.t list, Pool_message.Error.t) result
    =
    let open CCResult in
    let open Assignment in
    Logs.info ~src (fun m -> m "Handle command Cancel" ~tags);
    let contact =
      assignments |> CCList.hd |> fun ({ contact; _ } : t) -> contact
    in
    let* (_ : unit list) =
      let* () = Session.assignments_cancelable session in
      CCList.map is_cancellable assignments |> CCList.all_ok
    in
    let cancel_events =
      CCList.fold_left
        (fun acc assignment ->
          let updated =
            { assignment with canceled_at = Some (CanceledAt.create_now ()) }
          in
          let changelog =
            let open VersionHistory in
            create
              ~entity_uuid:(Id.to_common assignment.id)
              ~user_uuid:(Admin.id admin |> Admin.Id.to_common)
              ~before:(assignment |> to_record)
              ~after:(updated |> to_record)
              ()
            |> Common.changelog_event
          in
          acc @ [ Canceled updated |> Pool_event.assignment ] @ changelog)
        []
        assignments
    in
    let decrease_assignment_count =
      Contact_counter.update_on_assignment_cancellation assignments contact
      |> Contact.updated
      |> Pool_event.contact
    in
    Ok
      (cancel_events
       @ [ decrease_assignment_count ]
       @ [ Email.sent notification_email |> Pool_event.email ])
  ;;

  let effects = Assignment.Guard.Access.update
end

module CreateFromWaitingList : sig
  include Common.CommandSig

  type t =
    { session : Session.t
    ; follow_up_sessions : Session.t list
    ; waiting_list : Waiting_list.t
    ; already_enrolled : bool
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Assignment.t -> Email.dispatch)
    -> (Pool_event.t list, Pool_message.Error.t) result

  val effects : Experiment.Id.t -> Pool_common.Id.t -> Guard.ValidationSet.t
end = struct
  type t =
    { session : Session.t
    ; follow_up_sessions : Session.t list
    ; waiting_list : Waiting_list.t
    ; already_enrolled : bool
    }

  let handle
    ?(tags = Logs.Tag.empty)
    ({ session; follow_up_sessions; waiting_list; already_enrolled } : t)
    confirmation_email
    =
    let all_sessions = session :: follow_up_sessions in
    Logs.info ~src (fun m -> m "Handle command CreateFromWaitingList" ~tags);
    let open CCResult in
    if already_enrolled
    then Error Pool_message.Error.AlreadySignedUpForExperiment
    else (
      let contact = waiting_list.Waiting_list.contact in
      let* creation_events =
        assignment_creation_and_confirmation_events
          confirmation_email
          session
          follow_up_sessions
          contact
      in
      let conter_events =
        Contact_counter.update_on_assignment_from_waiting_list
          contact
          all_sessions
        |> Contact.updated
        |> Pool_event.contact
      in
      Ok (creation_events @ [ conter_events ]))
  ;;

  let effects experiment_id waiting_list_id =
    let open Guard.ValidationSet in
    And
      [ Waiting_list.Guard.Access.update experiment_id waiting_list_id
      ; Assignment.Guard.Access.create experiment_id
      ]
  ;;
end

module MarkAsDeleted : sig
  include
    Common.CommandSig
    with type t =
      Contact.t * Assignment.t list * Assignment.IncrementParticipationCount.t

  val handle
    :  ?tags:Logs.Tag.set
    -> Admin.t
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val effects : Experiment.Id.t -> Assignment.Id.t -> Guard.ValidationSet.t
end = struct
  type t =
    Contact.t * Assignment.t list * Assignment.IncrementParticipationCount.t

  let handle
    ?(tags = Logs.Tag.empty)
    admin
    (contact, assignments, decrement_participation_count)
    : (Pool_event.t list, Pool_message.Error.t) result
    =
    let open Assignment in
    let open CCResult in
    Logs.info ~src (fun m -> m ~tags "Handle command MarkAsDeleted");
    let* (_ : unit list) =
      CCList.map is_deletable assignments |> CCList.all_ok
    in
    let mark_as_deleted =
      CCList.fold_left
        (fun acc assignment ->
          let changelog =
            let open VersionHistory in
            create
              ~entity_uuid:(Id.to_common assignment.id)
              ~user_uuid:(Admin.id admin |> Admin.Id.to_common)
              ~before:(assignment |> to_record)
              ~after:
                ({ assignment with
                   marked_as_deleted = MarkedAsDeleted.create true
                 }
                 |> to_record)
              ()
            |> Common.changelog_event
          in
          acc
          @ [ assignment |> markedasdeleted |> Pool_event.assignment ]
          @ changelog)
        []
        assignments
    in
    let contact_updated =
      Contact_counter.update_on_assignment_deletion
        assignments
        contact
        decrement_participation_count
      |> Contact.updated
      |> Pool_event.contact
    in
    Ok (contact_updated :: mark_as_deleted)
  ;;

  let effects = Assignment.Guard.Access.delete
end

module Update : sig
  type t = update

  val handle
    :  ?tags:Logs.Tag.set
    -> Admin.t
    -> Experiment.t
    -> [ `Session of Session.t | `TimeWindow of Time_window.t ]
    -> Assignment.t
    -> bool
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
end = struct
  type t = update

  let handle
    ?(tags = Logs.Tag.empty)
    admin
    (experiment : Experiment.t)
    session
    ({ Assignment.no_show; participated; _ } as assignment)
    participated_in_other_assignments
    (command : update)
    =
    Logs.info ~src (fun m -> m "Handle command Update" ~tags);
    let open CCResult in
    let open Assignment in
    let current_values, updated_values =
      let open Contact_counter in
      let current_values =
        match session with
        | `Session { Session.closed_at; _ } ->
          (match CCOption.is_some closed_at, no_show, participated with
           | true, Some no_show, Some participated ->
             Some { no_show; participated }
           | _, _, _ -> None)
        | `TimeWindow _ ->
          (match no_show, participated with
           | Some no_show, Some participated -> Some { no_show; participated }
           | _, _ -> None)
      in
      let updated_values =
        { no_show = command.no_show; participated = command.participated }
      in
      current_values, updated_values
    in
    let contact_counters =
      match current_values with
      | Some current_values ->
        Contact_counter.update_on_assignment_update
          assignment
          ~current_values
          ~updated_values
          ~participated_in_other_assignments
        |> Contact.updated
        |> Pool_event.contact
        |> CCList.return
      | _ -> []
    in
    let updated =
      { assignment with
        no_show = Some command.no_show
      ; participated = Some command.participated
      ; external_data_id = command.external_data_id
      }
    in
    let changelog =
      let open VersionHistory in
      create
        ~entity_uuid:(Id.to_common assignment.id)
        ~user_uuid:(Admin.id admin |> Admin.Id.to_common)
        ~before:(assignment |> to_record)
        ~after:(updated |> to_record)
        ()
      |> Common.changelog_event
    in
    let* () =
      validate experiment updated
      |> function
      | Ok () | Error [] -> Ok ()
      | Error (hd :: _) -> Error hd
    in
    Ok
      (((Assignment.Updated updated |> Pool_event.assignment)
        :: contact_counters)
       @ changelog)
  ;;

  let decode data =
    Conformist.decode_and_validate update_schema data
    |> CCResult.map_err Pool_message.to_conformist_error
  ;;
end

type update_htmx =
  | ExternalDataId of Assignment.ExternalDataId.t option
  | Participated of Assignment.Participated.t
  | NoShow of Assignment.NoShow.t
[@@deriving show, eq, variants]

module UpdateHtmx : sig
  type t = update_htmx

  val handle : ?tags:Logs.Tag.set -> Assignment.t -> t -> Assignment.t
  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
end = struct
  type t = update_htmx

  let handle ?(tags = Logs.Tag.empty) (assignment : Assignment.t) =
    let open Assignment in
    Logs.info ~src (fun m -> m "Handle command UpdateHtmx" ~tags);
    function
    | ExternalDataId external_data_id -> { assignment with external_data_id }
    | Participated participated ->
      let no_show =
        match Participated.value participated with
        | true -> false |> NoShow.create |> CCOption.return
        | false -> assignment.no_show
      in
      { assignment with participated = Some participated; no_show }
    | NoShow no_show ->
      let participated =
        match NoShow.value no_show with
        | true -> false |> Participated.create |> CCOption.return
        | false -> assignment.participated
      in
      { assignment with participated; no_show = Some no_show }
  ;;

  let decode data =
    let open Pool_message in
    let open Assignment in
    let decode_bool decode str =
      str |> Utils.Bool.of_string |> decode |> CCResult.return
    in
    let decode_data_id = function
      | "" -> Ok None
      | str -> str |> ExternalDataId.create |> CCResult.map CCOption.return
    in
    let fields =
      [ Field.ExternalDataId, decode_data_id %> CCResult.map externaldataid
      ; Field.Participated, decode_bool (Participated.create %> participated)
      ; Field.NoShow, decode_bool (NoShow.create %> noshow)
      ]
    in
    let open CCOption in
    CCList.find_map
      (fun (field, decoder) ->
        CCList.assoc_opt ~eq:( = ) (Field.show field) data
        >>= CCList.head_opt
        >|= decoder)
      fields
    |> function
    | None -> Error Error.InvalidHtmxRequest
    | Some res -> res
  ;;
end

module SendReminder : sig
  include Common.CommandSig with type t = Pool_common.MessageChannel.t

  val handle
    :  ?tags:Logs.Tag.set
    -> (Assignment.t -> (Email.dispatch, Pool_message.Error.t) result)
       * (Assignment.t
          -> Pool_user.CellPhone.t
          -> (Text_message.job, Pool_message.Error.t) result)
    -> Session.t
    -> Assignment.t
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode : Conformist.input -> (t, Pool_message.Error.t) result
  val effects : Experiment.Id.t -> Session.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Pool_common.MessageChannel.t

  let schema =
    Conformist.(make Field.[ Pool_common.MessageChannel.schema () ] CCFun.id)
  ;;

  let handle
    ?(tags = Logs.Tag.empty)
    (create_email, create_text_message)
    session
    ({ Assignment.contact; _ } as assignment)
    channel
    =
    Logs.info ~src (fun m -> m "Handle command ResendReminders" ~tags);
    let open Pool_common.Reminder in
    let open CCResult.Infix in
    let* () = Assignment.reminder_sendable session assignment in
    let* msg_event =
      let open Pool_common.MessageChannel in
      match channel with
      | Email -> assignment |> create_email >|= Email.sent >|= Pool_event.email
      | TextMessage ->
        (match contact.Contact.cell_phone with
         | None -> Error Pool_message.(Error.Missing Field.CellPhone)
         | Some cell_phone ->
           create_text_message assignment cell_phone
           >|= Text_message.sent
           >|= Pool_event.text_message)
    in
    let update =
      Assignment.(
        Updated
          { assignment with
            reminder_manually_last_sent_at = Some (SentAt.create_now ())
          })
      |> Pool_event.assignment
    in
    Ok [ msg_event; update ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let effects exp_id = Session.Guard.Access.update exp_id
end

type session_swap =
  { session : Session.Id.t
  ; notify_contact : Pool_common.NotifyContact.t
  ; language : Pool_common.Language.t
  ; email_subject : Message_template.EmailSubject.t
  ; email_text : Message_template.EmailText.t
  ; plain_text : Message_template.PlainText.t
  }

let session_schema () =
  let open Session in
  Pool_conformist.schema_decoder
    CCFun.(Id.of_string %> CCResult.return)
    Id.value
    Pool_message.Field.Session
;;

module SwapSession : sig
  val handle
    :  ?tags:Logs.Tag.set
    -> ?assignment_id:Assignment.Id.t
    -> current_session:Session.t
    -> new_session:Session.t
    -> Assignment.t
    -> Email.dispatch option
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode
    :  (string * string list) list
    -> (session_swap, Pool_message.Error.t) result
end = struct
  let handle
    ?(tags = Logs.Tag.empty)
    ?assignment_id
    ~current_session
    ~new_session
    assignment
    notification_email
    : (Pool_event.t list, Pool_message.Error.t) result
    =
    let open Assignment in
    let open CCResult in
    Logs.info ~src (fun m -> m ~tags "Handle command SwapSession");
    let* () = Session.can_be_assigned_to_existing_assignment new_session in
    let* () = session_changeable current_session assignment in
    let new_assignment =
      { assignment with
        id = CCOption.value ~default:(Assignment.Id.create ()) assignment_id
      }
    in
    let email_event =
      notification_email
      |> CCOption.map_or
           ~default:[]
           CCFun.(Email.sent %> Pool_event.email %> CCList.return)
    in
    Ok
      ([ MarkedAsDeleted assignment |> Pool_event.assignment
       ; Created (new_assignment, new_session.Session.id)
         |> Pool_event.assignment
       ]
       @ email_event)
  ;;

  let command
    session
    notify_contact
    language
    email_subject
    email_text
    plain_text
    =
    { session; notify_contact; language; email_subject; email_text; plain_text }
  ;;

  let schema =
    let open Message_template in
    Pool_conformist.(
      make
        Field.
          [ session_schema ()
          ; Pool_common.NotifyContact.schema ()
          ; Pool_common.Language.schema ()
          ; EmailSubject.schema ()
          ; EmailText.schema ()
          ; PlainText.schema ()
          ]
        command)
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_message.to_conformist_error
  ;;
end

module UpdateMatchesFilter : sig
  include
    Common.CommandSig with type t = Assignment.event list * Email.dispatch list

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val effects : Experiment.Id.t -> Session.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Assignment.event list * Email.dispatch list

  let handle ?(tags = Logs.Tag.empty) (assignment_events, emails) =
    Logs.info ~src (fun m -> m "Handle command UpdateMatchesFilter" ~tags);
    Ok
      ((assignment_events |> CCList.map Pool_event.assignment)
       @ [ Email.BulkSent emails |> Pool_event.email ])
  ;;

  let effects id = Session.Guard.Access.update id
end

module OnlineSurvey = struct
  module Create : sig
    include Common.CommandSig

    type t =
      { contact : Contact.t
      ; time_window : Time_window.t
      ; experiment : Experiment.Public.t
      }

    val handle
      :  ?tags:Logs.Tag.set
      -> ?id:Assignment.Id.t
      -> t
      -> (Pool_event.t list, Pool_message.Error.t) result

    val effects : Experiment.Id.t -> Guard.ValidationSet.t
  end = struct
    type t =
      { contact : Contact.t
      ; time_window : Time_window.t
      ; experiment : Experiment.Public.t
      }

    let handle ?(tags = Logs.Tag.empty) ?id { contact; time_window; experiment }
      =
      Logs.info ~src (fun m -> m "Handle command OnlineSurvey.Create" ~tags);
      let open CCResult in
      let open Pool_message.Error in
      let* () =
        if Contact.is_inactive contact then Error ContactIsInactive else Ok ()
      in
      let* () =
        let open Experiment in
        experiment
        |> Public.direct_registration_disabled
        |> DirectRegistrationDisabled.value
        |> Utils.bool_to_result_not DirectRegistrationIsDisabled
      in
      let assignment_event =
        let open Assignment in
        (create ?id contact, time_window.Time_window.id)
        |> created
        |> Pool_event.assignment
      in
      let contact_event =
        let open Contact in
        update_num_assignments ~step:1 contact |> updated |> Pool_event.contact
      in
      Ok [ assignment_event; contact_event ]
    ;;

    let effects = Assignment.Guard.Access.create
  end

  type submit = { external_data_id : Assignment.ExternalDataId.t option }

  module Submit : sig
    include Common.CommandSig

    type t = submit

    val decode : (string * string list) list -> (t, Pool_message.Error.t) result

    val handle
      :  ?tags:Logs.Tag.set
      -> Assignment.t
      -> t
      -> (Pool_event.t list, Pool_message.Error.t) result

    val effects : Experiment.Id.t -> Guard.ValidationSet.t
  end = struct
    type t = submit

    let command external_data_id = { external_data_id }

    let schema =
      let open Assignment in
      Conformist.(
        make Field.[ Conformist.optional @@ ExternalDataId.schema () ] command)
    ;;

    let decode data =
      Conformist.decode_and_validate schema data
      |> CCResult.map_err Pool_message.to_conformist_error
    ;;

    let handle
      ?(tags = Logs.Tag.empty)
      ({ Assignment.contact; participated; marked_as_deleted; _ } as assignment)
      ({ external_data_id } : t)
      =
      Logs.info ~src (fun m -> m "Handle command OnlineSurvey.Submit" ~tags);
      let open CCResult in
      let open Assignment in
      let* () =
        if MarkedAsDeleted.value marked_as_deleted
        then Error Pool_message.(Error.NotFound Field.Assignment)
        else Ok ()
      in
      let* () =
        if CCOption.is_some participated
        then Error Pool_message.Error.AssignmentAlreadySubmitted
        else Ok ()
      in
      let assignment_event =
        { assignment with
          external_data_id
        ; no_show = Some (NoShow.create false)
        ; participated = Some (Participated.create true)
        }
        |> updated
        |> Pool_event.assignment
      in
      let contact_event =
        let open Contact in
        contact
        |> update_num_participations ~step:1
        |> update_num_show_ups ~step:1
        |> updated
        |> Pool_event.contact
      in
      Ok [ assignment_event; contact_event ]
    ;;

    let effects = Assignment.Guard.Access.create
  end
end
