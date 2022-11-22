module Conformist = Pool_common.Utils.PoolConformist

let src = Logs.Src.create "settings.cqrs"

let effects action tenant =
  [ ( action
    , `Target
        (tenant.Pool_tenant.id |> Guard.Uuid.target_of Pool_common.Id.value) )
  ; action, `TargetEntity `Setting
  ]
;;

module UpdateLanguages : sig
  include Common.CommandSig with type t = Pool_common.Language.t list

  val handle
    :  ?tags:Logs.Tag.set
    -> Settings.TermsAndConditions.t list
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Pool_tenant.t -> Guard.Authorizer.effect list
end = struct
  type t = Pool_common.Language.t list

  let handle ?(tags = Logs.Tag.empty) terms command =
    Logs.info ~src (fun m -> m "Handle command UpdateLanguage" ~tags);
    let open CCResult in
    match CCList.length command > 0 with
    | false -> Error Pool_common.Message.(NoOptionSelected Field.Language)
    | true ->
      let open CCResult.Infix in
      let terms = CCList.map Settings.TermsAndConditions.value terms in
      CCList.map
        (fun l ->
          CCList.assoc_opt ~eq:Pool_common.Language.equal l terms
          |> CCOption.to_result Pool_common.Message.TermsAndConditionsMissing)
        command
      |> CCResult.flatten_l
      >>= CCFun.const
            (Ok [ Settings.LanguagesUpdated command |> Pool_event.settings ])
  ;;

  let effects = effects `Update
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

  val effects : Pool_tenant.t -> Guard.Authorizer.effect list
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

  let effects = effects `Update
end

module UpdateEmailSuffixes : sig
  include Common.CommandSig with type t = (string * string list) list

  val effects : Pool_tenant.t -> Guard.Authorizer.effect list
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

  let effects = effects `Update
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

  val effects : Pool_tenant.t -> Guard.Authorizer.effect list
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

  let effects = effects `Delete
end

module UpdateContactEmail : sig
  include Common.CommandSig with type t = Settings.ContactEmail.t

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects : Pool_tenant.t -> Guard.Authorizer.effect list
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

  let effects = effects `Update
end

module InactiveUser = struct
  module DisableAfter : sig
    include Common.CommandSig with type t = Settings.InactiveUser.DisableAfter.t

    val decode
      :  (string * string list) list
      -> (t, Pool_common.Message.error) result

    val effects : Pool_tenant.t -> Guard.Authorizer.effect list
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

    let effects = effects `Update
  end

  module Warning : sig
    include Common.CommandSig with type t = Settings.InactiveUser.Warning.t

    val decode
      :  (string * string list) list
      -> (t, Pool_common.Message.error) result

    val effects : Pool_tenant.t -> Guard.Authorizer.effect list
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

    let effects = effects `Update
  end
end

module UpdateTriggerProfileUpdateAfter : sig
  include Common.CommandSig with type t = Settings.TriggerProfileUpdateAfter.t

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects : Pool_tenant.t -> Guard.Authorizer.effect list
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

  let effects = effects `Update
end

module UpdateTermsAndConditions : sig
  include Common.CommandSig with type t = (string * string list) list

  val handle
    :  ?tags:Logs.Tag.set
    -> Pool_common.Language.t list
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Pool_tenant.t -> Guard.Authorizer.effect list
end = struct
  type t = (string * string list) list

  let handle ?(tags = Logs.Tag.empty) tenant_languages urlencoded =
    Logs.info ~src (fun m -> m "Handle command UpdateTermsAndConditions" ~tags);
    let open CCResult in
    let ignore_emtpy =
      CCList.filter_map (fun (lang, terms) ->
        match lang, terms with
        | _, None | _, Some "" | "_csrf", _ -> None
        | _, Some terms -> Some (lang, terms))
    in
    let tenant_languages_are_set terms =
      let result =
        CCResult.flatten_l
        @@ CCList.map
             (fun tenant_language ->
               CCList.assoc_opt
                 ~eq:CCString.equal
                 (tenant_language |> Pool_common.Language.show)
                 terms
               |> CCOption.to_result ())
             tenant_languages
      in
      match result with
      | Error _ -> Error Pool_common.Message.RequestRequiredFields
      | Ok _ -> Ok terms
    in
    let* terms_and_conditions =
      urlencoded
      |> CCList.map (fun (l, t) -> l, CCList.head_opt t)
      |> ignore_emtpy
      |> tenant_languages_are_set
      >>= fun urlencoded ->
      CCResult.flatten_l
        (CCList.map
           (CCFun.uncurry Settings.TermsAndConditions.create)
           urlencoded)
    in
    Ok
      [ Settings.TermsAndConditionsUpdated terms_and_conditions
        |> Pool_event.settings
      ]
  ;;

  let effects = effects `Update
end

module RestoreDefault : sig
  include Common.CommandSig with type t = Pool_tenant.t

  val handle
    :  ?tags:Logs.Tag.set
    -> unit
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Pool_tenant.t -> Guard.Authorizer.effect list
end = struct
  type t = Pool_tenant.t

  let handle ?(tags = Logs.Tag.empty) () =
    Logs.info ~src (fun m -> m "Handle command RestoreDefault" ~tags);
    Ok [ Settings.(DefaultRestored default_values) |> Pool_event.settings ]
  ;;

  let effects = effects `Manage
end

module UpdateDefaultLeadTime : sig
  include Common.CommandSig with type t = Pool_common.Reminder.LeadTime.t

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects : Pool_tenant.t -> Guard.Authorizer.effect list
end = struct
  type t = Pool_common.Reminder.LeadTime.t

  let command contact_email = contact_email

  let schema =
    Conformist.(make Field.[ Pool_common.Reminder.LeadTime.schema () ] command)
  ;;

  let handle ?(tags = Logs.Tag.empty) contact_email =
    Logs.info ~src (fun m -> m "Handle command UpdateDefaultLeadTime" ~tags);
    Ok
      [ Settings.DefaultReminderLeadTimeUpdated contact_email
        |> Pool_event.settings
      ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects = effects `Update
end
