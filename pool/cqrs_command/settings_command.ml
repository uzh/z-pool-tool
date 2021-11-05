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
  type t = { email_suffix : Settings.EmailSuffix.t }

  val handle
    :  Settings.EmailSuffix.t list
    -> t
    -> (Pool_event.t list, string) Result.t

  val decode
    :  (string * string list) list
    -> (t, Conformist.error list) Result.t

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = { email_suffix : Settings.EmailSuffix.t }

  let command email_suffix = { email_suffix }

  let schema =
    Conformist.(make Field.[ Settings.EmailSuffix.schema () ] command)
  ;;

  let handle suffixes command =
    let suffixes = suffixes @ [ command.email_suffix ] in
    Ok [ Settings.EmailSuffixesUpdated suffixes |> Pool_event.settings ]
  ;;

  let can = Utils.todo
  let decode data = Conformist.decode_and_validate schema data
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
  type t = { contact_email : Settings.ContactEmail.t }

  val handle : t -> (Pool_event.t list, string) Result.t

  val decode
    :  (string * string list) list
    -> (t, Conformist.error list) Result.t

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = { contact_email : Settings.ContactEmail.t }

  let command contact_email = { contact_email }

  let schema =
    Conformist.(make Field.[ Settings.ContactEmail.schema () ] command)
  ;;

  let handle command =
    Ok
      [ Settings.ContactEmailUpdated command.contact_email
        |> Pool_event.settings
      ]
  ;;

  let can = Utils.todo
  let decode data = Conformist.decode_and_validate schema data
end
