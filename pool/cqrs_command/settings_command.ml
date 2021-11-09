module UpdateLanguages : sig
  type t = (string * string list) list

  val handle : t -> (Pool_event.t list, string) Result.t
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = (string * string list) list

  let handle command =
    let open CCResult in
    let* languages =
      CCList.filter_map
        (fun (k, v) ->
          match CCList.hd v with
          | "true" -> Some (Settings.Language.of_string k)
          | _ -> None)
        command
      |> CCResult.flatten_l
    in
    Ok [ Settings.LanguagesUpdated languages |> Pool_event.settings ]
  ;;

  let can = Utils.todo
end

module CreateEmailSuffixes : sig
  type t = Settings.EmailSuffix.t

  val handle
    :  Settings.EmailSuffix.t list
    -> t
    -> (Pool_event.t list, string) Result.t

  val decode : (string * string list) list -> (t, string) Result.t
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
    |> CCResult.map_err Utils.handle_conformist_error
  ;;
end

module UpdateEmailSuffixes : sig
  type t = (string * string list) list

  val handle : t -> (Pool_event.t list, string) Result.t
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

module UpdateContactEmail : sig
  type t = Settings.ContactEmail.t

  val handle : t -> (Pool_event.t list, string) Result.t
  val decode : (string * string list) list -> (t, string) Result.t
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
    |> CCResult.map_err Utils.handle_conformist_error
  ;;
end

module InactiveUser = struct
  module DisableAfter : sig
    type t = Settings.InactiveUser.DisableAfter.t

    val handle : t -> (Pool_event.t list, string) Result.t
    val decode : (string * string list) list -> (t, string) Result.t
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
      |> CCResult.map_err Utils.handle_conformist_error
    ;;
  end

  module Warning : sig
    type t = Settings.InactiveUser.Warning.t

    val handle : t -> (Pool_event.t list, string) Result.t
    val decode : (string * string list) list -> (t, string) Result.t
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
      |> CCResult.map_err Utils.handle_conformist_error
    ;;
  end
end

module UpdateTermsAndConditions : sig
  type t = Settings.TermsAndConditions.t

  val handle : t -> (Pool_event.t list, string) Result.t
  val decode : (string * string list) list -> (t, string) Result.t
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = Settings.TermsAndConditions.t

  let command terms_and_conditions = terms_and_conditions

  let schema =
    Conformist.(make Field.[ Settings.TermsAndConditions.schema () ] command)
  ;;

  let handle terms_and_conditions =
    Ok
      [ Settings.TermsAndConditionsUpdated terms_and_conditions
        |> Pool_event.settings
      ]
  ;;

  let can = Utils.todo

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Utils.handle_conformist_error
  ;;
end
