module Conformist = Pool_conformist

type validation_set = Guard.ValidationSet.t

open Settings
open CCFun.Infix

let src = Logs.Src.create "settings.cqrs"

let validated_gtx_api_key ~tags urlencoded =
  let open Gtx_config in
  let open Utils.Lwt_result.Infix in
  let schema =
    Conformist.(
      make
        Field.[ ApiKey.schema (); Sender.schema (); Pool_user.CellPhone.schema () ]
        (fun key sender number -> key, number, sender))
  in
  Conformist.decode_and_validate schema urlencoded
  |> Lwt_result.lift
  >|- Pool_message.to_conformist_error
  >>= fun (api_key, phone_nr, sender) ->
  Text_message.Service.test_api_key ~tags api_key phone_nr sender
;;

module UpdateLanguages : sig
  include Common.CommandSig with type t = Pool_common.Language.t list

  val handle : ?tags:Logs.Tag.set -> t -> (Pool_event.t list, Pool_message.Error.t) result
end = struct
  type t = Pool_common.Language.t list

  let handle ?(tags = Logs.Tag.empty) command =
    Logs.info ~src (fun m -> m "Handle command UpdateLanguage" ~tags);
    match CCList.length command > 0 with
    | false -> Error Pool_message.(Error.NoOptionSelected Field.Language)
    | true -> Ok [ Settings.LanguagesUpdated command |> Pool_event.settings ]
  ;;

  let effects = Settings.Guard.Access.update
end

module CreateEmailSuffix : sig
  include Common.CommandSig with type t = Settings.EmailSuffix.t

  val handle
    :  ?tags:Logs.Tag.set
    -> Settings.EmailSuffix.t list
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
end = struct
  type t = Settings.EmailSuffix.t

  let command email_suffix = email_suffix
  let schema = Conformist.(make Field.[ Settings.EmailSuffix.schema () ] command)

  let handle ?(tags = Logs.Tag.empty) suffixes email_suffix =
    Logs.info ~src (fun m -> m "Handle command CreateEmailSuffix" ~tags);
    let suffixes = suffixes @ [ email_suffix ] in
    Ok [ Settings.EmailSuffixesUpdated suffixes |> Pool_event.settings ]
  ;;

  let decode =
    Conformist.decode_and_validate schema
    %> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let effects = Settings.Guard.Access.update
end

module UpdateEmailSuffixes : sig
  include Common.CommandSig with type t = (string * string list) list
end = struct
  type t = (string * string list) list

  let handle ?(tags = Logs.Tag.empty) suffixes =
    Logs.info ~src (fun m -> m "Handle command UpdateEmailSuffixes" ~tags);
    let open CCResult in
    let* suffixes =
      CCList.filter_map
        (fun (key, v) ->
           if CCString.equal key "_csrf"
           then None
           else Some (Settings.EmailSuffix.create (CCList.hd v)))
        suffixes
      |> CCResult.flatten_l
    in
    Ok [ Settings.EmailSuffixesUpdated suffixes |> Pool_event.settings ]
  ;;

  let effects = Settings.Guard.Access.update
end

module DeleteEmailSuffix : sig
  include Common.CommandSig with type t = Settings.EmailSuffix.t

  val handle
    :  ?tags:Logs.Tag.set
    -> Settings.EmailSuffix.t list
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
end = struct
  type t = Settings.EmailSuffix.t

  let command email_suffix = email_suffix
  let schema = Conformist.(make Field.[ Settings.EmailSuffix.schema () ] command)

  let handle ?(tags = Logs.Tag.empty) suffixes email_suffix =
    Logs.info ~src (fun m -> m "Handle command DeleteEmailSuffix" ~tags);
    let suffixes =
      CCList.filter (fun s -> not (Settings.EmailSuffix.equal s email_suffix)) suffixes
    in
    Ok [ Settings.EmailSuffixesUpdated suffixes |> Pool_event.settings ]
  ;;

  let decode =
    Conformist.decode_and_validate schema
    %> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let effects = Settings.Guard.Access.update
end

module UpdateContactEmail : sig
  include Common.CommandSig with type t = Settings.ContactEmail.t

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
end = struct
  type t = Settings.ContactEmail.t

  let command contact_email = contact_email
  let schema = Conformist.(make Field.[ Settings.ContactEmail.schema () ] command)

  let handle ?(tags = Logs.Tag.empty) contact_email =
    Logs.info ~src (fun m -> m "Handle command UpdateContactEmail" ~tags);
    Ok [ Settings.ContactEmailUpdated contact_email |> Pool_event.settings ]
  ;;

  let decode =
    Conformist.decode_and_validate schema
    %> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let effects = Settings.Guard.Access.update
end

module UpdatePageScript : sig
  type t = PageScript.t option

  val decode
    :  PageScript.location
    -> (string * string list) list
    -> (t, Pool_message.Error.t) result

  val handle
    :  ?tags:Logs.Tag.set
    -> ?system_event_id:System_event.Id.t
    -> PageScript.location
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val effects : validation_set
end = struct
  type t = PageScript.t option

  let schema field =
    Conformist.(make Field.[ Conformist.optional @@ PageScript.schema field () ] CCFun.id)
  ;;

  let handle ?(tags = Logs.Tag.empty) ?system_event_id placement script =
    Logs.info ~src (fun m -> m "Handle command PageScript" ~tags);
    Ok
      [ Settings.PageScriptUpdated (script, placement) |> Pool_event.settings
      ; System_event.(Job.I18nPageUpdated |> create ?id:system_event_id |> created)
        |> Pool_event.system_event
      ]
  ;;

  let decode context =
    let field =
      let open Pool_message in
      let open Settings.PageScript in
      match context with
      | Head -> Field.PageScriptsHead
      | Body -> Field.PageScriptsBody
    in
    Conformist.decode_and_validate (schema field)
    %> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let effects = Settings.Guard.Access.update
end

module InactiveUser = struct
  module DisableAfter : sig
    include Common.CommandSig with type t = command

    val decode : (string * string list) list -> (t, Pool_message.Error.t) result
  end = struct
    open InactiveUser.DisableAfter

    type t = command

    let handle ?(tags = Logs.Tag.empty) { time_value; time_unit } =
      let open CCResult in
      Logs.info ~src (fun m -> m "Handle command DisableAfter" ~tags);
      let* inactive_user_disable_after = of_int time_value time_unit in
      Ok
        [ Settings.InactiveUserDisableAfterUpdated inactive_user_disable_after
          |> Pool_event.settings
        ]
    ;;

    let decode =
      Conformist.decode_and_validate (update_duration_schema (integer_schema ()) name)
      %> CCResult.map_err Pool_message.to_conformist_error
    ;;

    let effects = Settings.Guard.Access.update
  end

  module Warning : sig
    type t = (string * string list) list

    val handle
      :  ?tags:Logs.Tag.set
      -> values:string list
      -> units:string list
      -> unit
      -> (Pool_event.t list, Pool_message.Error.t) result

    val effects : validation_set
  end = struct
    type t = (string * string list) list

    let handle ?(tags = Logs.Tag.empty) ~values ~units () =
      Logs.info ~src (fun m -> m "Handle command WarnAfter" ~tags);
      let open CCList in
      let open CCResult.Infix in
      let* values =
        map
          (CCInt.of_string
           %> CCOption.to_result Pool_message.(Error.Invalid Field.TimeSpan))
          values
        |> all_ok
      in
      let* timespans =
        values
        |> mapi (fun i value ->
          nth_opt units i
          |> CCOption.to_result Pool_message.(Error.Missing Field.TimeUnit)
          >>= Pool_model.Base.TimeUnit.of_string
          >>= Settings.InactiveUser.Warning.TimeSpan.of_int value)
        |> all_ok
      in
      Ok [ Settings.InactiveUserWarningUpdated timespans |> Pool_event.settings ]
    ;;

    let effects = Settings.Guard.Access.update
  end

  module DisableService : sig
    type t = Settings.InactiveUser.ServiceDisabled.t

    val handle
      :  ?tags:Logs.Tag.set
      -> t
      -> (Pool_event.t list, Pool_message.Error.t) result

    val decode : (string * string list) list -> (t, Pool_message.Error.t) result
    val effects : validation_set
  end = struct
    type t = Settings.InactiveUser.ServiceDisabled.t

    let handle ?(tags = Logs.Tag.empty) disabled =
      Logs.info ~src (fun m -> m "Handle command DisableService" ~tags);
      Ok [ Settings.InactiveUserServiceDisabled disabled |> Pool_event.settings ]
    ;;

    let schema =
      Conformist.(make Field.[ Settings.InactiveUser.ServiceDisabled.schema () ] CCFun.id)
    ;;

    let decode data =
      Pool_conformist.decode_and_validate schema data
      |> CCResult.map_err Pool_message.to_conformist_error
    ;;

    let effects = Settings.Guard.Access.update
  end
end

module UpdateTriggerProfileUpdateAfter : sig
  include Common.CommandSig with type t = command

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
end = struct
  open TriggerProfileUpdateAfter

  type t = command

  let handle ?(tags = Logs.Tag.empty) { time_value; time_unit } =
    let open CCResult in
    Logs.info ~src (fun m -> m "Handle command UpdateTriggerProfileUpdateAfter" ~tags);
    let* trigger_warning_after = of_int time_value time_unit in
    Ok
      [ Settings.TriggerProfileUpdateAfterUpdated trigger_warning_after
        |> Pool_event.settings
      ]
  ;;

  let decode =
    Conformist.decode_and_validate (update_duration_schema (integer_schema ()) name)
    %> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let effects = Settings.Guard.Access.update
end

module UpdateDefaultEmailLeadTime : sig
  include Common.CommandSig with type t = command

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
end = struct
  open Pool_common.Reminder.EmailLeadTime

  type t = command

  let handle ?(tags = Logs.Tag.empty) { time_value; time_unit } =
    let open CCResult in
    Logs.info ~src (fun m -> m "Handle command UpdateDefaultEmailLeadTime" ~tags);
    let* email_lead_time = of_int time_value time_unit in
    Ok [ Settings.DefaultReminderLeadTimeUpdated email_lead_time |> Pool_event.settings ]
  ;;

  let decode =
    Conformist.decode_and_validate (update_duration_schema (integer_schema ()) name)
    %> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let effects = Settings.Guard.Access.update
end

module UpdateDefaultTextMessageLeadTime : sig
  include Common.CommandSig with type t = command

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
end = struct
  open Pool_common.Reminder.TextMessageLeadTime

  type t = command

  let handle ?(tags = Logs.Tag.empty) { time_value; time_unit } =
    let open CCResult in
    Logs.info ~src (fun m -> m "Handle command UpdateDefaultTextMessageLeadTime" ~tags);
    let* lead_time = of_int time_value time_unit in
    Ok [ Settings.DefaultTextMsgReminderLeadTimeUpdated lead_time |> Pool_event.settings ]
  ;;

  let decode =
    Conformist.decode_and_validate (update_duration_schema (integer_schema ()) name)
    %> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let effects = Settings.Guard.Access.update
end

module CreateGtxApiKey : sig
  include Common.CommandSig with type t = Gtx_config.ApiKey.t * Gtx_config.Sender.t

  val handle
    :  ?tags:Logs.Tag.set
    -> ?id:Gtx_config.Id.t
    -> ?system_event_id:System_event.Id.t
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result
end = struct
  open Gtx_config

  type t = ApiKey.t * Sender.t

  let handle
        ?(tags = Logs.Tag.empty)
        ?(id = Id.create ())
        ?system_event_id
        (api_key, sender)
    =
    Logs.info ~src (fun m -> m "Handle command CreateGtxApiKey" ~tags);
    let config = create ~id api_key sender in
    let open Pool_event in
    Ok
      [ Created config |> gtx_config
      ; System_event.(create ?id:system_event_id Job.GtxConfigCacheCleared |> created)
        |> system_event
      ]
  ;;

  let effects = Guard.Access.create
end

module UpdateGtxApiKey : sig
  include Common.CommandSig with type t = Gtx_config.ApiKey.t * Gtx_config.Sender.t

  val handle
    :  ?tags:Logs.Tag.set
    -> ?system_event_id:System_event.Id.t
    -> Gtx_config.t
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result
end = struct
  open Gtx_config

  type t = ApiKey.t * Sender.t

  let handle ?(tags = Logs.Tag.empty) ?system_event_id config (api_key, sender) =
    Logs.info ~src (fun m -> m "Handle command UpdateGtxApiKey" ~tags);
    let updated = { config with api_key; sender } in
    let open Pool_event in
    Ok
      [ Updated (config, updated) |> gtx_config
      ; System_event.(create ?id:system_event_id Job.GtxConfigCacheCleared |> created)
        |> system_event
      ]
  ;;

  let effects = Guard.Access.update
end

module RemoveGtxApiKey : sig
  val handle
    :  ?tags:Logs.Tag.set
    -> ?system_event_id:System_event.Id.t
    -> unit
    -> (Pool_event.t list, Pool_message.Error.t) result

  val effects : validation_set
end = struct
  let handle ?(tags = Logs.Tag.empty) ?system_event_id () =
    let open Gtx_config in
    Logs.info ~src (fun m -> m "Handle command RemoveGtxApiKey" ~tags);
    let open Pool_event in
    Ok
      [ Removed |> gtx_config
      ; System_event.(create ?id:system_event_id Job.GtxConfigCacheCleared |> created)
        |> system_event
      ]
  ;;

  let effects = Guard.Access.update
end

module UserImportReminder = struct
  module UpdateFirstReminder : sig
    include Common.CommandSig with type t = command

    val decode : (string * string list) list -> (t, Pool_message.Error.t) result
  end = struct
    open UserImportReminder.FirstReminderAfter

    type t = command

    let handle ?(tags = Logs.Tag.empty) { time_value; time_unit } =
      let open CCResult in
      Logs.info ~src (fun m -> m "Handle command UpdateFirstReminder" ~tags);
      let* first_reminder_after = of_int time_value time_unit >>= validate in
      Ok
        [ Settings.UserImportFirstReminderAfterUpdated first_reminder_after
          |> Pool_event.settings
        ]
    ;;

    let decode =
      Conformist.decode_and_validate (update_duration_schema (integer_schema ()) name)
      %> CCResult.map_err Pool_message.to_conformist_error
    ;;

    let effects = Settings.Guard.Access.update
  end

  module UpdateSecondReminder : sig
    include Common.CommandSig with type t = command

    val decode : (string * string list) list -> (t, Pool_message.Error.t) result
  end = struct
    open UserImportReminder.SecondReminderAfter

    type t = command

    let handle ?(tags = Logs.Tag.empty) { time_value; time_unit } =
      let open CCResult in
      Logs.info ~src (fun m -> m "Handle command UpdateSecondReminder" ~tags);
      let* second_reminder_after = of_int time_value time_unit >>= validate in
      Ok
        [ Settings.UserImportSecondReminderAfterUpdated second_reminder_after
          |> Pool_event.settings
        ]
    ;;

    let decode =
      Conformist.decode_and_validate (update_duration_schema (integer_schema ()) name)
      %> CCResult.map_err Pool_message.to_conformist_error
    ;;

    let effects = Settings.Guard.Access.update
  end
end
