module Conformist = Pool_common.Utils.PoolConformist

module UpdateLanguages : sig
  type t = Pool_common.Language.t list

  val handle
    :  Settings.TermsAndConditions.t list
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = Pool_common.Language.t list

  let handle terms command =
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

  let can = Utils.todo
end

module CreateEmailSuffix : sig
  type t = Settings.EmailSuffix.t

  val handle
    :  Settings.EmailSuffix.t list
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = Settings.EmailSuffix.t

  let command email_suffix = email_suffix

  let schema =
    Conformist.(make Field.[ Settings.EmailSuffix.schema () ] command)
  ;;

  let handle suffixes email_suffix =
    let suffixes = suffixes @ [ email_suffix ] in
    Ok [ Settings.EmailSuffixesUpdated suffixes |> Pool_event.settings ]
  ;;

  let can = Utils.todo

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;
end

module UpdateEmailSuffixes : sig
  type t = (string * string list) list

  val handle : t -> (Pool_event.t list, Pool_common.Message.error) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = (string * string list) list

  let handle suffixes =
    let open CCResult in
    let* suffixes =
      CCList.map
        (fun (_, v) -> Settings.EmailSuffix.create (CCList.hd v))
        suffixes
      |> CCResult.flatten_l
    in
    Ok [ Settings.EmailSuffixesUpdated suffixes |> Pool_event.settings ]
  ;;

  let can = Utils.todo
end

module DeleteEmailSuffix : sig
  type t = Settings.EmailSuffix.t

  val handle
    :  Settings.EmailSuffix.t list
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = Settings.EmailSuffix.t

  let command email_suffix = email_suffix

  let schema =
    Conformist.(make Field.[ Settings.EmailSuffix.schema () ] command)
  ;;

  let handle suffixes email_suffix =
    let suffixes =
      CCList.filter
        (fun s -> not (Settings.EmailSuffix.equal s email_suffix))
        suffixes
    in
    Ok [ Settings.EmailSuffixesUpdated suffixes |> Pool_event.settings ]
  ;;

  let can = Utils.todo

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;
end

module UpdateContactEmail : sig
  type t = Settings.ContactEmail.t

  val handle : t -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = Settings.ContactEmail.t

  let command contact_email = contact_email

  let schema =
    Conformist.(make Field.[ Settings.ContactEmail.schema () ] command)
  ;;

  let handle contact_email =
    Ok [ Settings.ContactEmailUpdated contact_email |> Pool_event.settings ]
  ;;

  let can = Utils.todo

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;
end

module InactiveUser = struct
  module DisableAfter : sig
    type t = Settings.InactiveUser.DisableAfter.t

    val handle : t -> (Pool_event.t list, Pool_common.Message.error) result

    val decode
      :  (string * string list) list
      -> (t, Pool_common.Message.error) result

    val can : Sihl_user.t -> t -> bool Lwt.t
  end = struct
    type t = Settings.InactiveUser.DisableAfter.t

    let command inactive_user_disable_after = inactive_user_disable_after

    let schema =
      Conformist.(
        make Field.[ Settings.InactiveUser.DisableAfter.schema () ] command)
    ;;

    let handle inactive_user_disable_after =
      Ok
        [ Settings.InactiveUserDisableAfterUpdated inactive_user_disable_after
          |> Pool_event.settings
        ]
    ;;

    let can = Utils.todo

    let decode data =
      Conformist.decode_and_validate schema data
      |> CCResult.map_err Pool_common.Message.to_conformist_error
    ;;
  end

  module Warning : sig
    type t = Settings.InactiveUser.Warning.t

    val handle : t -> (Pool_event.t list, Pool_common.Message.error) result

    val decode
      :  (string * string list) list
      -> (t, Pool_common.Message.error) result

    val can : Sihl_user.t -> t -> bool Lwt.t
  end = struct
    type t = Settings.InactiveUser.Warning.t

    let command inactive_user_warning = inactive_user_warning

    let schema =
      Conformist.(
        make Field.[ Settings.InactiveUser.Warning.schema () ] command)
    ;;

    let handle inactive_user_warning =
      Ok
        [ Settings.InactiveUserWarningUpdated inactive_user_warning
          |> Pool_event.settings
        ]
    ;;

    let can = Utils.todo

    let decode data =
      Conformist.decode_and_validate schema data
      |> CCResult.map_err Pool_common.Message.to_conformist_error
    ;;
  end
end

module UpdateTermsAndConditions : sig
  type t = (string * string list) list

  val handle
    :  Pool_common.Language.t list
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = (string * string list) list

  let handle tenant_languages urlencoded =
    let open CCResult in
    let ignore_emtpy =
      CCList.filter_map (fun (lang, terms) ->
          match terms with
          | None | Some "" -> None
          | Some terms -> Some (lang, terms))
    in
    let tenant_languages_are_set terms =
      let result =
        CCResult.flatten_l
        @@ CCList.map
             (fun tenant_language ->
               CCList.assoc_opt
                 ~eq:CCString.equal
                 (tenant_language |> Pool_common.Language.code)
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

  let can = Utils.todo
end

module RestoreDefault : sig
  type t = Pool_tenant.t

  val handle : unit -> (Pool_event.t list, Pool_common.Message.error) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = Pool_tenant.t

  let handle () =
    Ok [ Settings.(DefaultRestored default_values) |> Pool_event.settings ]
  ;;

  let can user command =
    Permission.can
      user
      ~any_of:
        [ Permission.Destroy
            (Permission.Tenant, Some (command |> Pool_tenant.id))
        ]
  ;;
end
