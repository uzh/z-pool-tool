open Experiment
module Conformist = Pool_common.Utils.PoolConformist
module Id = Pool_common.Id

let default_schema command =
  Pool_common.Utils.PoolConformist.(
    make
      Field.
        [ Title.schema ()
        ; PublicTitle.schema ()
        ; Description.schema ()
        ; DirectRegistrationDisabled.schema ()
        ; RegistrationDisabled.schema ()
        ; AllowUninvitedSignup.schema ()
        ; Conformist.optional @@ Pool_common.ExperimentType.schema ()
        ; Conformist.optional @@ InvitationTemplate.Subject.schema ()
        ; Conformist.optional @@ InvitationTemplate.Text.schema ()
        ; Conformist.optional @@ Pool_common.Reminder.LeadTime.schema ()
        ; Conformist.optional @@ Pool_common.Reminder.Subject.schema ()
        ; Conformist.optional @@ Pool_common.Reminder.Text.schema ()
        ]
      command)
;;

let default_command
  title
  public_title
  description
  direct_registration_disabled
  registration_disabled
  allow_uninvited_signup
  experiment_type
  invitation_subject
  invitation_text
  session_reminder_lead_time
  session_reminder_subject
  session_reminder_text
  =
  { title
  ; public_title
  ; description
  ; direct_registration_disabled
  ; registration_disabled
  ; allow_uninvited_signup
  ; experiment_type
  ; invitation_subject
  ; invitation_text
  ; session_reminder_lead_time
  ; session_reminder_subject
  ; session_reminder_text
  }
;;

module Create : sig
  type t = create

  val handle : t -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects : Ocauth.Authorizer.effect list
end = struct
  type t = create

  let handle (command : t) =
    let open CCResult in
    let* experiment =
      Experiment.create
        command.title
        command.public_title
        command.description
        command.direct_registration_disabled
        command.registration_disabled
        command.allow_uninvited_signup
        command.experiment_type
        command.invitation_subject
        command.invitation_text
        command.session_reminder_lead_time
        command.session_reminder_subject
        command.session_reminder_text
    in
    Ok [ Experiment.Created experiment |> Pool_event.experiment ]
  ;;

  let decode data =
    Conformist.decode_and_validate (default_schema default_command) data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects = [ `Create, `Role `Experiment ]
end

module Update : sig
  type t = create

  val handle
    :  Experiment.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects : Experiment.t -> Ocauth.Authorizer.effect list
end = struct
  type t = create

  let handle experiment (command : t) =
    let open CCResult in
    let* experiment =
      Experiment.create
        ~id:experiment.Experiment.id
        command.title
        command.public_title
        command.description
        command.direct_registration_disabled
        command.registration_disabled
        command.allow_uninvited_signup
        command.experiment_type
        command.invitation_subject
        command.invitation_text
        command.session_reminder_lead_time
        command.session_reminder_subject
        command.session_reminder_text
    in
    Ok [ Experiment.Updated experiment |> Pool_event.experiment ]
  ;;

  let decode data =
    Conformist.decode_and_validate (default_schema default_command) data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects experiment =
    [ `Update, `Uniq (experiment.id |> Pool_common.Id.to_uuidm) ]
  ;;
end

module Delete : sig
  type t =
    { experiment_id : Id.t
    ; session_count : int
    }

  val handle : t -> (Pool_event.t list, Pool_common.Message.error) result
  val effects : t -> Ocauth.Authorizer.effect list
end = struct
  (* Only when no sessions added *)

  type t =
    { experiment_id : Id.t
    ; session_count : int
    }

  let handle { experiment_id; session_count } =
    match session_count > 0 with
    | true -> Error Pool_common.Message.ExperimentSessionCountNotZero
    | false ->
      Ok [ Experiment.Destroyed experiment_id |> Pool_event.experiment ]
  ;;

  let effects command =
    [ `Delete, `Uniq (command.experiment_id |> Pool_common.Id.to_uuidm) ]
  ;;
end

module UpdateFilter : sig
  type t = Filter.filter

  val handle
    :  Experiment.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Experiment.t -> Ocauth.Authorizer.effect list
end = struct
  type t = Filter.filter

  let handle experiment filter =
    match experiment.filter with
    | None ->
      let id = Pool_common.Id.create () in
      let filter = Filter.create ~id filter in
      let experiment = Experiment.{ experiment with filter = Some filter } in
      Ok
        [ Filter.Created filter |> Pool_event.filter
        ; Experiment.Updated experiment |> Pool_event.experiment
        ]
    | Some current_filter ->
      let filter = Filter.{ current_filter with filter } in
      Ok [ Filter.Updated filter |> Pool_event.filter ]
  ;;

  let effects experiment =
    [ `Update, `Uniq (experiment.id |> Pool_common.Id.to_uuidm) ]
  ;;
end

module AddExperimenter : sig
  type t = { user_id : Id.t }

  val handle
    :  Experiment.t
    -> Admin.experimenter Admin.t
    -> (Pool_event.t list, 'a) result

  val effects
    :  Experiment.t
    -> Admin.experimenter Admin.t
    -> Ocauth.Authorizer.effect list
end = struct
  type t = { user_id : Id.t }

  let handle experiment user =
    Ok
      [ Experiment.ExperimenterAssigned (experiment, user)
        |> Pool_event.experiment
      ]
  ;;

  let effects experiment user =
    [ `Update, `Uniq (experiment.id |> Pool_common.Id.to_uuidm)
    ; `Update, `Uniq (Ocauth.Uuid.of_string_exn (Admin.user user).Sihl_user.id)
    ]
  ;;
end

module DivestExperimenter : sig
  type t =
    { user_id : Id.t
    ; experiment_id : Id.t
    }

  val handle
    :  Experiment.t
    -> Admin.experimenter Admin.t
    -> (Pool_event.t list, 'a) result

  val effects : t -> Ocauth.Authorizer.effect list
end = struct
  type t =
    { user_id : Id.t
    ; experiment_id : Id.t
    }

  let handle experiment user =
    Ok
      [ Experiment.ExperimenterDivested (experiment, user)
        |> Pool_event.experiment
      ]
  ;;

  let effects { user_id; experiment_id } =
    [ `Update, `Uniq (experiment_id |> Pool_common.Id.to_uuidm)
    ; `Update, `Uniq (user_id |> Pool_common.Id.to_uuidm)
    ]
  ;;
end

module AddAssistant : sig
  type t = { user_id : Id.t }

  val handle
    :  Experiment.t
    -> Admin.assistant Admin.t
    -> (Pool_event.t list, 'a) result

  val effects : Experiment.t -> t -> Ocauth.Authorizer.effect list
end = struct
  type t = { user_id : Id.t }

  let handle experiment user =
    Ok
      [ Experiment.AssistantAssigned (experiment, user) |> Pool_event.experiment
      ]
  ;;

  let effects experiment t =
    [ `Update, `Uniq (experiment.Experiment.id |> Pool_common.Id.to_uuidm)
    ; `Update, `Uniq (t.user_id |> Pool_common.Id.to_uuidm)
    ]
  ;;
end

module DivestAssistant : sig
  type t =
    { user_id : Id.t
    ; experiment_id : Id.t
    }

  val handle
    :  Experiment.t
    -> Admin.assistant Admin.t
    -> (Pool_event.t list, 'a) result

  val effects
    :  Experiment.t
    -> Admin.experimenter Admin.t
    -> Ocauth.Authorizer.effect list
end = struct
  type t =
    { user_id : Id.t
    ; experiment_id : Id.t
    }

  let handle experiment user =
    Ok
      [ Experiment.AssistantDivested (experiment, user) |> Pool_event.experiment
      ]
  ;;

  let effects experiment user =
    [ `Update, `Uniq (experiment.id |> Pool_common.Id.to_uuidm)
    ; `Update, `Uniq (Ocauth.Uuid.of_string_exn (Admin.user user).Sihl_user.id)
    ]
  ;;
end
