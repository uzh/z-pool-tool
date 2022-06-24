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
    (* TODO: Validate session reminder info*)
    if max_participants >= min_participants
    then (
      (* TODO[timhub] add validation *)
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
        [ Session.Created (session, experiment_id, location)
          |> Pool_event.session
        ])
    else
      Error
        Pool_common.Message.(
          Smaller (Field.MaxParticipants, Field.MinParticipants))
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let can user _ =
    Permission.can user ~any_of:[ Permission.Manage (Permission.System, None) ]
  ;;
end

module Update = struct
  type t = Session.base

  let command = session_command
  let schema = session_schema

  let handle
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
    if max_participants >= min_participants
    then (
      (* TODO[timhub] add validation *)
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
        [ Session.Updated (session_cmd, location, session) |> Pool_event.session
        ])
    else
      Error
        Pool_common.Message.(
          Smaller (Field.MaxParticipants, Field.MinParticipants))
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let can user _ =
    Permission.can user ~any_of:[ Permission.Manage (Permission.System, None) ]
  ;;
end

module Delete : sig
  type t = { session : Session.t }

  val handle
    :  Session.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = { session : Session.t }

  let handle session =
    (* TODO [aerben] only when no assignments added *)
    Ok [ Session.Deleted session |> Pool_event.session ]
  ;;

  let can user _ =
    Permission.can user ~any_of:[ Permission.Manage (Permission.System, None) ]
  ;;
end

module Cancel : sig
  type t =
    { session : Session.t
    ; notify_via : string
    }

  val handle
    :  Session.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  (* TODO issue #90 step 2 *)
  (* notify_via: Email, SMS *)
  type t =
    { session : Session.t
    ; notify_via : string
    }

  let handle session = Ok [ Session.Canceled session |> Pool_event.session ]

  let can user _ =
    Permission.can user ~any_of:[ Permission.Manage (Permission.System, None) ]
  ;;
end

module SendReminder : sig
  type t = (Session.t * Sihl_email.t list) list

  val handle : t -> (Pool_event.t list, Pool_common.Message.error) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = (Session.t * Sihl_email.t list) list

  let handle command =
    Logs.info (fun m -> m "In session cqrs command: %i" (CCList.length command));
    let _ =
      CCList.map
        (fun (s, _) ->
          Logs.info (fun m -> m "%s" (s.Session.id |> Pool_common.Id.value)))
        command
    in
    Ok
      (CCList.map
         (fun data -> Session.ReminderSent data |> Pool_event.session)
         command)
  ;;

  let can user _ =
    Permission.can user ~any_of:[ Permission.Manage (Permission.System, None) ]
  ;;
end
