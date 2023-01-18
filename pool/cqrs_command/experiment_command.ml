module BaseGuard = Guard
open Experiment
module Conformist = Pool_common.Utils.PoolConformist

let src = Logs.Src.create "experiment_command.cqrs"

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
        ; Conformist.optional @@ Pool_common.Reminder.LeadTime.schema ()
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
  session_reminder_lead_time
  =
  { title
  ; public_title
  ; description
  ; direct_registration_disabled
  ; registration_disabled
  ; allow_uninvited_signup
  ; experiment_type
  ; session_reminder_lead_time
  }
;;

type update_role =
  { admin : Admin.t
  ; experiment : Experiment.t
  }

module Create : sig
  include Common.CommandSig with type t = create

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result
end = struct
  type t = create

  let handle ?(tags = Logs.Tag.empty) (command : t) =
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
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
        command.session_reminder_lead_time
    in
    Ok [ Experiment.Created experiment |> Pool_event.experiment ]
  ;;

  let decode data =
    Conformist.decode_and_validate (default_schema default_command) data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects = [ `Create, `TargetEntity `Experiment ]
end

module Update : sig
  include Common.CommandSig with type t = create

  val handle
    :  ?tags:Logs.Tag.set
    -> Experiment.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects : Id.t -> BaseGuard.Authorizer.effect list
end = struct
  type t = create

  let handle ?(tags = Logs.Tag.empty) experiment (command : t) =
    Logs.info ~src (fun m -> m "Handle command Update" ~tags);
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
        command.session_reminder_lead_time
    in
    Ok [ Experiment.Updated experiment |> Pool_event.experiment ]
  ;;

  let decode data =
    Conformist.decode_and_validate (default_schema default_command) data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects id =
    [ `Update, `Target (id |> BaseGuard.Uuid.target_of Experiment.Id.value)
    ; `Update, `TargetEntity `Experiment
    ]
  ;;
end

module Delete : sig
  include Common.CommandSig

  type t =
    { experiment : Experiment.t
    ; session_count : int
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Id.t -> BaseGuard.Authorizer.effect list
end = struct
  (* Only when no sessions added *)

  type t =
    { experiment : Experiment.t
    ; session_count : int
    }

  let handle ?(tags = Logs.Tag.empty) { experiment; session_count } =
    Logs.info ~src (fun m -> m "Handle command Delete" ~tags);
    match session_count > 0 with
    | true -> Error Pool_common.Message.ExperimentSessionCountNotZero
    | false ->
      let filter_event =
        experiment.Experiment.filter
        |> CCOption.map_or ~default:[] (fun f ->
             [ Filter.Deleted f |> Pool_event.filter ])
      in
      Ok
        ([ Experiment.Destroyed experiment.Experiment.id
           |> Pool_event.experiment
         ]
        @ filter_event)
  ;;

  let effects id =
    [ `Delete, `Target (id |> BaseGuard.Uuid.target_of Experiment.Id.value)
    ; `Delete, `TargetEntity `Experiment
    ]
  ;;
end

module UpdateFilter : sig
  include Common.CommandSig with type t = Filter.query

  val handle
    :  ?tags:Logs.Tag.set
    -> Experiment.t
    -> Filter.Key.human list
    -> Filter.t list
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Experiment.Id.t -> BaseGuard.Authorizer.effect list
end = struct
  type t = Filter.query

  let handle ?(tags = Logs.Tag.empty) experiment key_list template_list query =
    Logs.info ~src (fun m -> m "Handle command UpdateFilter" ~tags);
    let open CCResult in
    let* query = Filter.validate_query key_list template_list query in
    match experiment.filter with
    | None ->
      let id = Pool_common.Id.create () in
      let filter = Filter.create ~id None query in
      let experiment = Experiment.{ experiment with filter = Some filter } in
      Ok
        [ Filter.Created filter |> Pool_event.filter
        ; Experiment.Updated experiment |> Pool_event.experiment
        ]
    | Some current_filter ->
      let filter = Filter.{ current_filter with query } in
      Ok [ Filter.Updated filter |> Pool_event.filter ]
  ;;

  (* TODO: Make sure user has experiment AND filter permission *)
  let effects id =
    [ `Update, `Target (id |> BaseGuard.Uuid.target_of Id.value)
    ; `Update, `TargetEntity `Experiment
    ]
  ;;
end

module AssignAssistant : sig
  include Common.CommandSig with type t = update_role

  val effects : Experiment.Id.t -> BaseGuard.Authorizer.effect list
end = struct
  type t = update_role

  let handle ?(tags = Logs.Tag.empty) { admin; experiment } =
    Logs.info ~src (fun m -> m "Handle command AssignAssistant" ~tags);
    Ok
      [ Experiment.AssistantAssigned (experiment, admin)
        |> Pool_event.experiment
      ]
  ;;

  (* TODO[timhub]: Make sure both rules are true *)
  let effects id =
    [ `Update, `Target (id |> BaseGuard.Uuid.target_of Id.value)
    ; `Update, `TargetEntity (`Admin `Assistant)
    ]
  ;;
end

module DivestAssistant : sig
  include Common.CommandSig with type t = update_role

  val effects : Experiment.Id.t -> BaseGuard.Authorizer.effect list
end = struct
  type t = update_role

  let handle ?(tags = Logs.Tag.empty) { admin; experiment } =
    Logs.info ~src (fun m -> m "Handle command DivestAssistant" ~tags);
    Ok
      [ Experiment.AssistantDivested (experiment, admin)
        |> Pool_event.experiment
      ]
  ;;

  (* TODO[timhub]: Make sure both rules are true *)
  let effects id =
    [ `Update, `Target (id |> BaseGuard.Uuid.target_of Id.value)
    ; `Update, `TargetEntity (`Admin `Assistant)
    ]
  ;;
end

module AssignExperimenter : sig
  include Common.CommandSig with type t = update_role

  val effects : Experiment.Id.t -> BaseGuard.Authorizer.effect list
end = struct
  type t = update_role

  let handle ?(tags = Logs.Tag.empty) { admin; experiment } =
    Logs.info ~src (fun m -> m "Handle command AssignExperimenter" ~tags);
    Ok
      [ Experiment.ExperimenterAssigned (experiment, admin)
        |> Pool_event.experiment
      ]
  ;;

  (* TODO[timhub]: Make sure both rules are true *)
  let effects experiment_id =
    [ `Update, `Target (experiment_id |> BaseGuard.Uuid.target_of Id.value)
    ; `Update, `TargetEntity (`Admin `Experimenter)
    ]
  ;;
end

module DivestExperimenter : sig
  include Common.CommandSig with type t = update_role

  val effects : Experiment.Id.t -> BaseGuard.Authorizer.effect list
end = struct
  type t = update_role

  let handle ?(tags = Logs.Tag.empty) { admin; experiment } =
    Logs.info ~src (fun m -> m "Handle command DivestExperimenter" ~tags);
    Ok
      [ Experiment.ExperimenterDivested (experiment, admin)
        |> Pool_event.experiment
      ]
  ;;

  (* TODO[timhub]: Make sure both rules are true *)
  let effects experiment_id =
    [ `Update, `Target (experiment_id |> BaseGuard.Uuid.target_of Id.value)
    ; `Update, `TargetEntity (`Admin `Experimenter)
    ]
  ;;
end

module DeleteFilter : sig
  include Common.CommandSig with type t = Experiment.t

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Experiment.Id.t -> BaseGuard.Authorizer.effect list
end = struct
  type t = Experiment.t

  let handle ?(tags = Logs.Tag.empty) experiment =
    Logs.info ~src (fun m -> m "Handle command Delete" ~tags);
    let filter_event =
      experiment.Experiment.filter
      |> CCOption.map_or ~default:[] (fun f ->
           [ Filter.Deleted f |> Pool_event.filter ])
    in
    let experiment = Experiment.{ experiment with filter = None } in
    Ok
      ([ Experiment.Updated experiment |> Pool_event.experiment ] @ filter_event)
  ;;

  let effects experiment_id =
    [ `Update, `Target (experiment_id |> BaseGuard.Uuid.target_of Id.value)
    ; `Update, `TargetEntity (`Admin `Experimenter)
    ]
  ;;
end
