module BaseGuard = Guard
open Experiment
module Conformist = Pool_common.Utils.PoolConformist

let src = Logs.Src.create "experiment_command.cqrs"
let to_actor = CCFun.(Admin.id %> BaseGuard.Uuid.actor_of Admin.Id.value)
let to_target { id; _ } = BaseGuard.Uuid.target_of Id.value id
let to_role = BaseGuard.RoleSet.singleton

let default_schema command =
  Pool_common.Utils.PoolConformist.(
    make
      Field.
        [ Title.schema ()
        ; PublicTitle.schema ()
        ; Conformist.optional @@ Description.schema ()
        ; Conformist.optional @@ CostCenter.schema ()
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
  cost_center
  direct_registration_disabled
  registration_disabled
  allow_uninvited_signup
  experiment_type
  session_reminder_lead_time
  =
  { title
  ; public_title
  ; description
  ; cost_center
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

  val handle
    :  ?tags:Logs.Tag.set
    -> Admin.t option
    -> Organisational_unit.t option
    -> Email.SmtpAuth.t option
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result
end = struct
  type t = create

  let handle
    ?(tags = Logs.Tag.empty)
    contact_person
    organisational_unit
    smtp_auth
    (command : t)
    =
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
    let open CCResult in
    let* experiment =
      Experiment.create
        command.title
        command.public_title
        command.description
        command.cost_center
        organisational_unit
        (contact_person |> CCOption.map Admin.id)
        (smtp_auth |> CCOption.map Email.SmtpAuth.(fun ({ id; _ } : t) -> id))
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

  let effects = Experiment.Guard.Access.create
end

module Update : sig
  include Common.CommandSig with type t = create

  val handle
    :  ?tags:Logs.Tag.set
    -> Experiment.t
    -> Admin.t option
    -> Organisational_unit.t option
    -> Email.SmtpAuth.t option
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects : Id.t -> BaseGuard.ValidationSet.t
end = struct
  type t = create

  let handle
    ?(tags = Logs.Tag.empty)
    experiment
    contact_person
    organisational_unit
    smtp_auth
    (command : t)
    =
    Logs.info ~src (fun m -> m "Handle command Update" ~tags);
    let open CCResult in
    let* experiment =
      Experiment.create
        ~id:experiment.Experiment.id
        command.title
        command.public_title
        command.description
        command.cost_center
        organisational_unit
        (contact_person |> CCOption.map Admin.id)
        (smtp_auth |> CCOption.map Email.SmtpAuth.(fun ({ id; _ } : t) -> id))
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

  let effects = Experiment.Guard.Access.update
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
    -> ?system_event_id:System_event.Id.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Id.t -> BaseGuard.ValidationSet.t
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
    ?system_event_id
    { experiment
    ; session_count
    ; mailings
    ; experimenters
    ; assistants
    ; templates
    }
    =
    let open CCFun in
    let open CCResult in
    Logs.info ~src (fun m -> m "Handle command Delete" ~tags);
    let* () =
      session_count
      > 0
      |> Utils.bool_to_result_not
           Pool_common.Message.ExperimentSessionCountNotZero
    in
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
      ([ Experiment.Deleted experiment.Experiment.id |> Pool_event.experiment ]
       @ (experiment.Experiment.filter |> filter_events)
       @ (mailings |> CCList.map delete_mailing)
       @ (experimenters |> CCList.map revoke_experimenter)
       @ (assistants |> CCList.map revoke_assistant)
       @ (templates |> CCList.map delete_template)
       @ [ Common.guardian_cache_cleared_event ?id:system_event_id () ])
  ;;

  let effects = Experiment.Guard.Access.delete
end

module AssignAssistant : sig
  include Common.CommandSig with type t = update_role

  val effects : Id.t -> BaseGuard.ValidationSet.t
end = struct
  type t = update_role

  let handle ?(tags = Logs.Tag.empty) { admin; experiment } =
    Logs.info ~src (fun m -> m "Handle command AssignAssistant" ~tags);
    Ok
      [ BaseGuard.RolesGranted
          (admin |> to_actor, `Assistant (experiment |> to_target) |> to_role)
        |> Pool_event.guard
      ; Common.guardian_cache_cleared_event ()
      ]
  ;;

  let effects id =
    let open BaseGuard in
    let target_id = id |> Uuid.target_of Id.value in
    ValidationSet.(
      And
        [ Experiment.Guard.Access.update id
        ; Or
            [ SpecificRole `ManageAssistants
            ; SpecificRole (`ManageAssistant target_id)
            ]
        ])
  ;;
end

module UnassignAssistant : sig
  include Common.CommandSig with type t = update_role

  val effects : Id.t -> BaseGuard.ValidationSet.t
end = struct
  type t = update_role

  let handle ?(tags = Logs.Tag.empty) { admin; experiment } =
    Logs.info ~src (fun m -> m "Handle command UnassignAssistant" ~tags);
    Ok
      [ BaseGuard.RolesRevoked
          (admin |> to_actor, `Assistant (experiment |> to_target) |> to_role)
        |> Pool_event.guard
      ; Common.guardian_cache_cleared_event ()
      ]
  ;;

  let effects id =
    let open BaseGuard in
    let target_id = id |> Uuid.target_of Id.value in
    ValidationSet.(
      And
        [ Experiment.Guard.Access.update id
        ; Or
            [ SpecificRole `ManageAssistants
            ; SpecificRole (`ManageAssistant target_id)
            ]
        ])
  ;;
end

module AssignExperimenter : sig
  include Common.CommandSig with type t = update_role

  val effects : Id.t -> BaseGuard.ValidationSet.t
end = struct
  type t = update_role

  let handle ?(tags = Logs.Tag.empty) { admin; experiment } =
    Logs.info ~src (fun m -> m "Handle command AssignExperimenter" ~tags);
    Ok
      [ BaseGuard.RolesGranted
          ( admin |> to_actor
          , `Experimenter (experiment |> to_target)
            |> to_role
            |> BaseGuard.RoleSet.add
                 (`ManageAssistant (experiment |> to_target)) )
        |> Pool_event.guard
      ; Common.guardian_cache_cleared_event ()
      ]
  ;;

  let effects id =
    let open BaseGuard in
    let target_id = id |> Uuid.target_of Id.value in
    ValidationSet.(
      And
        [ Experiment.Guard.Access.update id
        ; Or
            [ SpecificRole `ManageExperimenters
            ; SpecificRole (`ManageExperimenter target_id)
            ]
        ])
  ;;
end

module UnassignExperimenter : sig
  include Common.CommandSig with type t = update_role

  val effects : Id.t -> BaseGuard.ValidationSet.t
end = struct
  type t = update_role

  let handle ?(tags = Logs.Tag.empty) { admin; experiment } =
    Logs.info ~src (fun m -> m "Handle command UnassignExperimenter" ~tags);
    Ok
      [ BaseGuard.RolesRevoked
          ( admin |> to_actor
          , `Experimenter (experiment |> to_target)
            |> to_role
            |> BaseGuard.RoleSet.add
                 (`ManageAssistant (experiment |> to_target)) )
        |> Pool_event.guard
      ; Common.guardian_cache_cleared_event ()
      ]
  ;;

  let effects id =
    let open BaseGuard in
    let target_id = id |> Uuid.target_of Id.value in
    ValidationSet.(
      And
        [ Experiment.Guard.Access.update id
        ; Or
            [ SpecificRole `ManageExperimenters
            ; SpecificRole (`ManageExperimenter target_id)
            ]
        ])
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

  val effects : Id.t -> BaseGuard.ValidationSet.t
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

  let effects id =
    BaseGuard.ValidationSet.And
      [ Experiment.Guard.Access.update id; Filter.Guard.Access.create ]
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

  val effects : Experiment.Id.t -> Filter.Id.t -> BaseGuard.ValidationSet.t
end = struct
  type t = Filter.query

  let handle ?(tags = Logs.Tag.empty) key_list template_list filter query =
    Logs.info ~src (fun m -> m "Handle command UpdateFilter" ~tags);
    let open CCResult in
    let* query = Filter.validate_query key_list template_list query in
    let filter = Filter.{ filter with query } in
    Ok [ Filter.Updated filter |> Pool_event.filter ]
  ;;

  let effects experiment_id filter_id =
    BaseGuard.ValidationSet.And
      [ Experiment.Guard.Access.update experiment_id
      ; Filter.Guard.Access.update filter_id
      ]
  ;;
end

module DeleteFilter : sig
  include Common.CommandSig with type t = Experiment.t

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Id.t -> Filter.Id.t -> BaseGuard.ValidationSet.t
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

  let effects experiment_id filter_id =
    BaseGuard.ValidationSet.And
      [ Experiment.Guard.Access.update experiment_id
      ; Filter.Guard.Access.delete filter_id
      ]
  ;;
end
