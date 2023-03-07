module BaseGuard = Guard
open Experiment
module Conformist = Pool_common.Utils.PoolConformist

let src = Logs.Src.create "experiment_command.cqrs"
let to_actor = CCFun.(Admin.id %> BaseGuard.Uuid.actor_of Admin.Id.value)
let to_target { id; _ } = BaseGuard.Uuid.target_of Id.value id
let to_role = BaseGuard.ActorRoleSet.singleton

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
    ; mailings : Mailing.t list
    ; assistants : Admin.t list
    ; experimenters : Admin.t list
    ; templates : Message_template.t list
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
    ; mailings : Mailing.t list
    ; assistants : Admin.t list
    ; experimenters : Admin.t list
    ; templates : Message_template.t list
    }

  let handle
    ?(tags = Logs.Tag.empty)
    { experiment
    ; session_count
    ; mailings
    ; experimenters
    ; assistants
    ; templates
    }
    =
    let open CCFun in
    Logs.info ~src (fun m -> m "Handle command Delete" ~tags);
    match session_count > 0 with
    | true -> Error Pool_common.Message.ExperimentSessionCountNotZero
    | false ->
      let delete_mailing = Mailing.deleted %> Pool_event.mailing in
      let revoke_experimenter admin =
        BaseGuard.RolesRevoked
          (admin |> to_actor, `Experimenter (experiment |> to_target) |> to_role)
        |> Pool_event.guard
      in
      let revoke_assistant admin =
        BaseGuard.RolesRevoked
          (admin |> to_actor, `Assistant (experiment |> to_target) |> to_role)
        |> Pool_event.guard
      in
      let filter_events =
        CCOption.map_or
          ~default:[]
          (Filter.deleted %> Pool_event.filter %> CCList.return)
      in
      let delete_template =
        Message_template.deleted %> Pool_event.message_template
      in
      Ok
        ([ Experiment.Deleted experiment.Experiment.id |> Pool_event.experiment
         ]
         @ (experiment.Experiment.filter |> filter_events)
         @ (mailings |> CCList.map delete_mailing)
         @ (experimenters |> CCList.map revoke_experimenter)
         @ (assistants |> CCList.map revoke_assistant)
         @ (templates |> CCList.map delete_template))
  ;;

  let effects id =
    [ `Delete, `Target (id |> BaseGuard.Uuid.target_of Experiment.Id.value)
    ; `Delete, `TargetEntity `Experiment
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
      [ BaseGuard.RolesGranted
          (admin |> to_actor, `Assistant (experiment |> to_target) |> to_role)
        |> Pool_event.guard
      ]
  ;;

  (* TODO[timhub]: Make sure both rules are true *)
  let effects id =
    [ `Update, `Target (id |> BaseGuard.Uuid.target_of Id.value)
    ; `Update, `TargetEntity (`Admin `Assistant)
    ]
  ;;
end

module UnassignAssistant : sig
  include Common.CommandSig with type t = update_role

  val effects : Experiment.Id.t -> BaseGuard.Authorizer.effect list
end = struct
  type t = update_role

  let handle ?(tags = Logs.Tag.empty) { admin; experiment } =
    Logs.info ~src (fun m -> m "Handle command UnassignAssistant" ~tags);
    Ok
      [ BaseGuard.RolesRevoked
          (admin |> to_actor, `Assistant (experiment |> to_target) |> to_role)
        |> Pool_event.guard
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
      [ BaseGuard.RolesGranted
          (admin |> to_actor, `Experimenter (experiment |> to_target) |> to_role)
        |> Pool_event.guard
      ]
  ;;

  (* TODO[timhub]: Make sure both rules are true *)
  let effects experiment_id =
    [ `Update, `Target (experiment_id |> BaseGuard.Uuid.target_of Id.value)
    ; `Update, `TargetEntity (`Admin `Experimenter)
    ]
  ;;
end

module UnassignExperimenter : sig
  include Common.CommandSig with type t = update_role

  val effects : Experiment.Id.t -> BaseGuard.Authorizer.effect list
end = struct
  type t = update_role

  let handle ?(tags = Logs.Tag.empty) { admin; experiment } =
    Logs.info ~src (fun m -> m "Handle command UnassignExperimenter" ~tags);
    Ok
      [ BaseGuard.RolesRevoked
          (admin |> to_actor, `Experimenter (experiment |> to_target) |> to_role)
        |> Pool_event.guard
      ]
  ;;

  (* TODO[timhub]: Make sure both rules are true *)
  let effects experiment_id =
    [ `Update, `Target (experiment_id |> BaseGuard.Uuid.target_of Id.value)
    ; `Update, `TargetEntity (`Admin `Experimenter)
    ]
  ;;
end

module CreateFilter : sig
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
    let id = Pool_common.Id.create () in
    let filter = Filter.create ~id None query in
    let experiment = Experiment.{ experiment with filter = Some filter } in
    Ok
      [ Filter.Created filter |> Pool_event.filter
      ; Experiment.Updated experiment |> Pool_event.experiment
      ]
  ;;

  (* TODO: Make sure authorization is inherited from experiment *)
  let effects id =
    [ `Manage, `Target (id |> BaseGuard.Uuid.target_of Experiment.Id.value)
    ; `Create, `TargetEntity `Filter
    ]
  ;;
end

module UpdateFilter : sig
  include Common.CommandSig with type t = Filter.query

  val handle
    :  ?tags:Logs.Tag.set
    -> Filter.Key.human list
    -> Filter.t list
    -> Filter.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Filter.Id.t -> BaseGuard.Authorizer.effect list
end = struct
  type t = Filter.query

  let handle ?(tags = Logs.Tag.empty) key_list template_list filter query =
    Logs.info ~src (fun m -> m "Handle command UpdateFilter" ~tags);
    let open CCResult in
    let* query = Filter.validate_query key_list template_list query in
    let filter = Filter.{ filter with query } in
    Ok [ Filter.Updated filter |> Pool_event.filter ]
  ;;

  (* TODO: Make sure authorization is inherited from experiment *)
  let effects id =
    [ `Update, `Target (id |> BaseGuard.Uuid.target_of Filter.Id.value)
    ; `Update, `TargetEntity `Filter
    ]
  ;;
end

module DeleteFilter : sig
  include Common.CommandSig with type t = Experiment.t

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Filter.Id.t -> BaseGuard.Authorizer.effect list
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

  (* TODO: Make sure authorization is inherited from experiment *)
  let effects id =
    [ `Delete, `Target (id |> BaseGuard.Uuid.target_of Filter.Id.value)
    ; `Delete, `TargetEntity `Filter
    ]
  ;;
end
