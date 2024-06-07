module Conformist = Pool_conformist
open Settings
open CCFun.Infix

let src = Logs.Src.create "settings.cqrs"

module UpdateLanguages : sig
  include Common.CommandSig with type t = Pool_common.Language.t list

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result
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

  let schema =
    Conformist.(make Field.[ Settings.EmailSuffix.schema () ] command)
  ;;

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

  let schema =
    Conformist.(make Field.[ Settings.EmailSuffix.schema () ] command)
  ;;

  let handle ?(tags = Logs.Tag.empty) suffixes email_suffix =
    Logs.info ~src (fun m -> m "Handle command DeleteEmailSuffix" ~tags);
    let suffixes =
      CCList.filter
        (fun s -> not (Settings.EmailSuffix.equal s email_suffix))
        suffixes
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

  let schema =
    Conformist.(make Field.[ Settings.ContactEmail.schema () ] command)
  ;;

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
      Conformist.decode_and_validate
        (update_duration_schema (integer_schema ()) name)
      %> CCResult.map_err Pool_message.to_conformist_error
    ;;

    let effects = Settings.Guard.Access.update
  end

  module Warning : sig
    include Common.CommandSig with type t = command

    val decode : (string * string list) list -> (t, Pool_message.Error.t) result
  end = struct
    open InactiveUser.Warning

    type t = command

    let handle ?(tags = Logs.Tag.empty) { time_value; time_unit } =
      let open CCResult in
      Logs.info ~src (fun m -> m "Handle command Warning" ~tags);
      let* inactive_user_warning = of_int time_value time_unit in
      Ok
        [ Settings.InactiveUserWarningUpdated inactive_user_warning
          |> Pool_event.settings
        ]
    ;;

    let decode =
      Conformist.decode_and_validate
        (update_duration_schema (integer_schema ()) name)
      %> CCResult.map_err Pool_message.to_conformist_error
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
    Logs.info ~src (fun m ->
      m "Handle command UpdateTriggerProfileUpdateAfter" ~tags);
    let* trigger_warning_after = of_int time_value time_unit in
    Ok
      [ Settings.TriggerProfileUpdateAfterUpdated trigger_warning_after
        |> Pool_event.settings
      ]
  ;;

  let decode =
    Conformist.decode_and_validate
      (update_duration_schema (integer_schema ()) name)
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
    Logs.info ~src (fun m ->
      m "Handle command UpdateDefaultEmailLeadTime" ~tags);
    let* email_lead_time = of_int time_value time_unit in
    Ok
      [ Settings.DefaultReminderLeadTimeUpdated email_lead_time
        |> Pool_event.settings
      ]
  ;;

  let decode =
    Conformist.decode_and_validate
      (update_duration_schema (integer_schema ()) name)
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
    Logs.info ~src (fun m ->
      m "Handle command UpdateDefaultTextMessageLeadTime" ~tags);
    let* lead_time = of_int time_value time_unit in
    Ok
      [ Settings.DefaultTextMsgReminderLeadTimeUpdated lead_time
        |> Pool_event.settings
      ]
  ;;

  let decode =
    Conformist.decode_and_validate
      (update_duration_schema (integer_schema ()) name)
    %> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let effects = Settings.Guard.Access.update
end

module UpdateGtxApiKey : sig
  include Common.CommandSig with type t = Pool_tenant.GtxApiKey.t

  val validated_gtx_api_key
    :  tags:Logs.Tag.set
    -> Pool_tenant.GtxSender.t
    -> Conformist.input
    -> (Pool_tenant.GtxApiKey.t, Pool_message.Error.t) Lwt_result.t

  val handle
    :  ?tags:Logs.Tag.set
    -> Pool_tenant.Write.t
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result
end = struct
  type t = Pool_tenant.GtxApiKey.t

  let validated_gtx_api_key ~tags sender urlencoded =
    let open Utils.Lwt_result.Infix in
    let schema =
      Conformist.(
        make
          Field.
            [ Pool_tenant.GtxApiKey.schema (); Pool_user.CellPhone.schema () ]
          CCPair.make)
    in
    Conformist.decode_and_validate schema urlencoded
    |> Lwt_result.lift
    >|- Pool_message.to_conformist_error
    >>= fun (api_key, phone_nr) ->
    Text_message.Service.test_api_key ~tags api_key phone_nr sender
  ;;

  let handle ?(tags = Logs.Tag.empty) tenant gtx_api_key =
    Logs.info ~src (fun m -> m "Handle command UpdateGtxApiKey" ~tags);
    Ok
      [ Pool_tenant.GtxApiKeyUpdated (tenant, gtx_api_key)
        |> Pool_event.pool_tenant
      ]
  ;;

  let effects = Guard.Access.update
end

module RemoveGtxApiKey : sig
  include Common.CommandSig with type t = Pool_tenant.Write.t

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result
end = struct
  type t = Pool_tenant.Write.t

  let handle ?(tags = Logs.Tag.empty) tenant =
    Logs.info ~src (fun m -> m "Handle command RemoveGtxApiKey" ~tags);
    Ok [ Pool_tenant.GtxApiKeyRemoved tenant |> Pool_event.pool_tenant ]
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
      Conformist.decode_and_validate
        (update_duration_schema (integer_schema ()) name)
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
      Conformist.decode_and_validate
        (update_duration_schema (integer_schema ()) name)
      %> CCResult.map_err Pool_message.to_conformist_error
    ;;

    let effects = Settings.Guard.Access.update
  end
end
