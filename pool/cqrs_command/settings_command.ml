module Conformist = Pool_common.Utils.PoolConformist

let src = Logs.Src.create "settings.cqrs"

module UpdateLanguages : sig
  include Common.CommandSig with type t = Pool_common.Language.t list

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result
end = struct
  type t = Pool_common.Language.t list

  let handle ?(tags = Logs.Tag.empty) command =
    Logs.info ~src (fun m -> m "Handle command UpdateLanguage" ~tags);
    match CCList.length command > 0 with
    | false -> Error Pool_common.Message.(NoOptionSelected Field.Language)
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
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result
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

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
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
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result
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

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects = Settings.Guard.Access.update
end

module UpdateContactEmail : sig
  include Common.CommandSig with type t = Settings.ContactEmail.t

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result
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

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects = Settings.Guard.Access.update
end

module InactiveUser = struct
  module DisableAfter : sig
    include Common.CommandSig with type t = Settings.InactiveUser.DisableAfter.t

    val decode
      :  (string * string list) list
      -> (t, Pool_common.Message.error) result
  end = struct
    type t = Settings.InactiveUser.DisableAfter.t

    let command inactive_user_disable_after = inactive_user_disable_after

    let schema =
      Conformist.(
        make Field.[ Settings.InactiveUser.DisableAfter.schema () ] command)
    ;;

    let handle ?(tags = Logs.Tag.empty) inactive_user_disable_after =
      Logs.info ~src (fun m -> m "Handle command DisableAfter" ~tags);
      Ok
        [ Settings.InactiveUserDisableAfterUpdated inactive_user_disable_after
          |> Pool_event.settings
        ]
    ;;

    let decode data =
      Conformist.decode_and_validate schema data
      |> CCResult.map_err Pool_common.Message.to_conformist_error
    ;;

    let effects = Settings.Guard.Access.update
  end

  module Warning : sig
    include Common.CommandSig with type t = Settings.InactiveUser.Warning.t

    val decode
      :  (string * string list) list
      -> (t, Pool_common.Message.error) result
  end = struct
    type t = Settings.InactiveUser.Warning.t

    let command inactive_user_warning = inactive_user_warning

    let schema =
      Conformist.(
        make Field.[ Settings.InactiveUser.Warning.schema () ] command)
    ;;

    let handle ?(tags = Logs.Tag.empty) inactive_user_warning =
      Logs.info ~src (fun m -> m "Handle command Warning" ~tags);
      Ok
        [ Settings.InactiveUserWarningUpdated inactive_user_warning
          |> Pool_event.settings
        ]
    ;;

    let decode data =
      Conformist.decode_and_validate schema data
      |> CCResult.map_err Pool_common.Message.to_conformist_error
    ;;

    let effects = Settings.Guard.Access.update
  end
end

module UpdateTriggerProfileUpdateAfter : sig
  include Common.CommandSig with type t = Settings.TriggerProfileUpdateAfter.t

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result
end = struct
  type t = Settings.TriggerProfileUpdateAfter.t

  let command trigger_update_after = trigger_update_after

  let schema =
    Conformist.(
      make Field.[ Settings.TriggerProfileUpdateAfter.schema () ] command)
  ;;

  let handle ?(tags = Logs.Tag.empty) inactive_user_warning =
    Logs.info ~src (fun m ->
      m "Handle command UpdateTriggerProfileUpdateAfter" ~tags);
    Ok
      [ Settings.TriggerProfileUpdateAfterUpdated inactive_user_warning
        |> Pool_event.settings
      ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects = Settings.Guard.Access.update
end

module UpdateDefaultLeadTime : sig
  type t = Pool_common.Reminder.LeadTime.t

  val handle
    :  ?tags:Logs.Tag.set
    -> [< `Email | `TextMessage ]
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects : Guard.ValidationSet.t
end = struct
  type t = Pool_common.Reminder.LeadTime.t

  let schema =
    Conformist.(make Field.[ Pool_common.Reminder.LeadTime.schema () ] CCFun.id)
  ;;

  let handle ?(tags = Logs.Tag.empty) key lead_time =
    Logs.info ~src (fun m -> m "Handle command UpdateDefaultLeadTime" ~tags);
    let event =
      match key with
      | `Email -> Settings.DefaultReminderLeadTimeUpdated lead_time
      | `TextMessage -> Settings.DefaultTextMsgReminderLeadTimeUpdated lead_time
    in
    Ok [ event |> Pool_event.settings ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects = Settings.Guard.Access.update
end
