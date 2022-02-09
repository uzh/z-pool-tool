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

  (* val build_checker : t -> ( actor:[ `User | `Admin ] Ocauth.Authorizable.t
     -> (unit, Conformist.error_msg) result , Pool_common.Message.error )
     Lwt_result.t *)
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

  (* let build_checker (_t : t) = let open Lwt_result.Syntax in let* rules =
     Ocauth.collect_rules [ `Create, `Role `Experiment ] in Lwt.return_ok
     (Ocauth.make_checker rules [@warning "-5"]) ;; *)
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

  (* val build_checker : Experiment.t -> ( actor:[ `User | `Admin ]
     Ocauth.Authorizable.t -> (unit, Conformist.error_msg) result ,
     Pool_common.Message.error ) Lwt_result.t *)
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

  (* let build_checker experiment = let open Lwt_result.Syntax in let* rules =
     Ocauth.collect_rules [ `Update, `Uniq (experiment.id |>
     Pool_common.Id.to_uuidm) ] in Lwt.return_ok (Ocauth.make_checker rules
     [@warning "-5"]) ;; *)
end

module Delete : sig
  type t =
    { experiment_id : Id.t
    ; session_count : int
    }

  val handle : t -> (Pool_event.t list, Pool_common.Message.error) result

  (* val build_checker : t -> ( actor:[ `User | `Admin ] Ocauth.Authorizable.t
     -> (unit, Conformist.error_msg) result , Pool_common.Message.error )
     Lwt_result.t *)
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

  (* let build_checker command = let open Lwt_result.Syntax in let* rules =
     Ocauth.collect_rules [ `Delete, `Uniq (command.experiment_id |>
     Pool_common.Id.to_uuidm) ] in Lwt.return_ok (Ocauth.make_checker rules
     [@warning "-5"]) ;; *)
end

module UpdateFilter : sig end = struct
  (* Update 'match_filter' flag in currently existing assignments *)
end

module AddExperimenter : sig
  type t = { user_id : Id.t }

  val handle
    :  Experiment.t
    -> Admin.experimenter Admin.t
    -> (Pool_event.t list, 'a) result

  (* val build_checker : Experiment.t -> Admin.experimenter Admin.t -> ( actor:[
     `User | `Admin ] Ocauth.Authorizable.t -> (unit, Conformist.error_msg)
     result , Pool_common.Message.error ) Lwt_result.t *)
end = struct
  type t = { user_id : Id.t }

  let handle experiment user =
    Ok
      [ Experiment.ExperimenterAssigned (experiment, user)
        |> Pool_event.experiment
      ]
  ;;

  (* let build_checker experiment user = let open Lwt_result.Syntax in let*
     rules = Ocauth.collect_rules [ `Update, `Uniq (experiment.id |>
     Pool_common.Id.to_uuidm) ; ( `Update , `Uniq (Ocauth.Uuid.of_string_exn
     (Admin.user user).Sihl_user.id) ) ] in Lwt.return_ok (Ocauth.make_checker
     rules [@warning "-5"]) ;; *)
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

  (* val build_checker : Experiment.t -> Admin.experimenter Admin.t -> ( actor:[
     `User | `Admin ] Ocauth.Authorizable.t -> (unit, Conformist.error_msg)
     result , Pool_common.Message.error ) Lwt_result.t *)
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

  (* let build_checker experiment user = let open Lwt_result.Syntax in let*
     rules = Ocauth.collect_rules [ `Update, `Uniq (experiment.id |>
     Pool_common.Id.to_uuidm) ; ( `Update , `Uniq (Ocauth.Uuid.of_string_exn
     (Admin.user user).Sihl_user.id) ) ] in Lwt.return_ok (Ocauth.make_checker
     rules [@warning "-5"]) ;; *)
end

module AddAssistant : sig
  type t = { user_id : Id.t }

  val handle
    :  Experiment.t
    -> Admin.assistant Admin.t
    -> (Pool_event.t list, 'a) result

  (* val build_checker : Experiment.t -> Admin.experimenter Admin.t -> ( actor:[
     `User | `Admin ] Ocauth.Authorizable.t -> (unit, Conformist.error_msg)
     result , Pool_common.Message.error ) Lwt_result.t *)
end = struct
  type t = { user_id : Id.t }

  let handle experiment user =
    Ok
      [ Experiment.AssistantAssigned (experiment, user) |> Pool_event.experiment
      ]
  ;;

  (* let build_checker experiment user = let open Lwt_result.Syntax in let*
     rules = Ocauth.collect_rules [ `Update, `Uniq (experiment.id |>
     Pool_common.Id.to_uuidm) ; ( `Update , `Uniq (Ocauth.Uuid.of_string_exn
     (Admin.user user).Sihl_user.id) ) ] in Lwt.return_ok (Ocauth.make_checker
     rules [@warning "-5"]) ;; *)
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

  (* val build_checker : Experiment.t -> Admin.experimenter Admin.t -> ( actor:[
     `User | `Admin ] Ocauth.Authorizable.t -> (unit, Conformist.error_msg)
     result , Pool_common.Message.error ) Lwt_result.t *)
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

  (* let build_checker experiment user = let open Lwt_result.Syntax in let*
     rules = Ocauth.collect_rules [ `Update, `Uniq (experiment.id |>
     Pool_common.Id.to_uuidm) ; ( `Update , `Uniq (Ocauth.Uuid.of_string_exn
     (Admin.user user).Sihl_user.id) ) ] in Lwt.return_ok (Ocauth.make_checker
     rules [@warning "-5"]) ;; *)
end
