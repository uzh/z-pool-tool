module Conformist = Pool_common.Utils.PoolConformist

let session_command
    start
    duration
    description
    max_participants
    min_participants
    overbook
    reminder_subject
    reminder_text
    reminder_lead_time
  =
  Session.
    { start
    ; duration
    ; description
    ; max_participants
    ; min_participants
    ; overbook
    ; reminder_subject
    ; reminder_text
    ; reminder_lead_time
    }
;;

let session_schema =
  Conformist.(
    make
      Field.
        [ Session.Start.schema ()
        ; Session.Duration.schema ()
        ; Conformist.optional @@ Session.Description.schema ()
        ; Session.ParticipantAmount.schema
            Pool_common.Message.Field.MaxParticipants
        ; Session.ParticipantAmount.schema
            Pool_common.Message.Field.MinParticipants
        ; Session.ParticipantAmount.schema Pool_common.Message.Field.Overbook
        ; Conformist.optional @@ Pool_common.Reminder.Subject.schema ()
        ; Conformist.optional @@ Pool_common.Reminder.Text.schema ()
        ; Conformist.optional @@ Pool_common.Reminder.LeadTime.schema ()
        ]
      session_command)
;;

(* TODO [aerben] create sigs *)
module Create = struct
  type t = Session.base

  let command = session_command
  let schema = session_schema

  let handle
      ?parent_session
      experiment_id
      location
      (Session.
         { start
         ; duration
         ; description
         ; max_participants
         ; min_participants
         ; (* TODO [aerben] find a better name *)
           overbook
         ; reminder_subject
         ; reminder_text
         ; reminder_lead_time
         } :
        Session.base)
    =
    (* If session is follow-up, make sure it's later than parent *)
    let follow_up_is_ealier =
      let open Session in
      CCOption.map_or
        ~default:false
        (fun (s : Session.t) ->
          Ptime.is_earlier ~than:(Start.value s.start) (Start.value start))
        parent_session
    in
    let validations =
      [ follow_up_is_ealier, Pool_common.Message.FollowUpIsEarlierThanMain
      ; ( max_participants < min_participants
        , Pool_common.Message.(
            Smaller (Field.MaxParticipants, Field.MinParticipants)) )
      ]
    in
    let open CCResult in
    let* () =
      validations
      |> CCList.filter fst
      |> CCList.map (fun (_, err) -> Error err)
      |> flatten_l
      |> map ignore
    in
    let (session : Session.base) =
      Session.
        { start
        ; duration
        ; description
        ; max_participants
        ; min_participants
        ; overbook
        ; reminder_subject
        ; reminder_text
        ; reminder_lead_time
        }
    in
    Ok
      [ Session.Created
          ( session
          , CCOption.map (fun s -> s.Session.id) parent_session
          , experiment_id
          , location )
        |> Pool_event.session
      ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects = [ `Manage, `Role `System ]
end

module Update = struct
  type t = Session.base

  let command = session_command
  let schema = session_schema

  let handle
      ?parent_session
      follow_up_sessions
      session
      location
      (Session.
         { start
         ; duration
         ; description
         ; max_participants
         ; min_participants
         ; overbook
         ; reminder_subject
         ; reminder_text
         ; reminder_lead_time
         } :
        Session.base)
    =
    (* If session has follow-ups, make sure they are all later *)
    let open Session in
    let follow_ups_are_ealier =
      CCList.exists
        (fun (follow_up : Session.t) ->
          Ptime.is_earlier
            ~than:(Start.value start)
            (Start.value follow_up.start))
        follow_up_sessions
    in
    (* If session is follow-up, make sure it's later than parent *)
    let follow_up_is_ealier =
      CCOption.map_or
        ~default:false
        (fun (s : Session.t) ->
          Ptime.is_earlier ~than:(Start.value s.start) (Start.value start))
        parent_session
    in
    print_endline @@ string_of_bool follow_up_is_ealier;
    let validations =
      [ ( follow_up_is_ealier || follow_ups_are_ealier
        , Pool_common.Message.FollowUpIsEarlierThanMain )
      ; ( max_participants < min_participants
        , Pool_common.Message.(
            Smaller (Field.MaxParticipants, Field.MinParticipants)) )
      ]
    in
    let open CCResult in
    let* () =
      validations
      |> CCList.filter fst
      |> CCList.map (fun (_, err) -> Error err)
      |> flatten_l
      |> map ignore
    in
    let (session_cmd : Session.base) =
      Session.
        { start
        ; duration
        ; description
        ; max_participants
        ; min_participants
        ; overbook
        ; reminder_subject
        ; reminder_text
        ; reminder_lead_time
        }
    in
    Ok
      [ Session.Updated (session_cmd, location, session) |> Pool_event.session ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects = [ `Manage, `Role `System ]
end

module Delete : sig
  type t = { session : Session.t }

  val handle
    :  Session.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Ocauth.Authorizer.effect list
end = struct
  type t = { session : Session.t }

  let handle session =
    (* TODO [aerben] only when no assignments added *)
    (* TODO [aerben] how to deal with follow-ups? currently they just disappear *)
    Ok [ Session.Deleted session |> Pool_event.session ]
  ;;

  let effects = [ `Manage, `Role `System ]
end

module Cancel : sig
  type t =
    { session : Session.t
    ; notify_via : string
    }

  val handle
    :  Session.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Ocauth.Authorizer.effect list
end = struct
  (* TODO issue #90 step 2 *)
  (* notify_via: Email, SMS *)
  type t =
    { session : Session.t
    ; notify_via : string
    }

  let handle session = Ok [ Session.Canceled session |> Pool_event.session ]
  let effects = [ `Manage, `Role `System ]
end

module SendReminder : sig
  type t = (Session.t * Sihl_email.t list) list

  val handle : t -> (Pool_event.t list, Pool_common.Message.error) result
  val effects : Ocauth.Authorizer.effect list
end = struct
  type t = (Session.t * Sihl_email.t list) list

  let handle command =
    Ok
      (CCList.map
         (fun data -> Session.ReminderSent data |> Pool_event.session)
         command)
  ;;

  let effects = [ `Manage, `Role `System ]
end
