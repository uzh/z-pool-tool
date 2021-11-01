module UpdateLanguages : sig
  type t = (string * string list) list

  val handle : t -> (Pool_event.t list, string) Result.t
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = (string * string list) list

  let handle languages =
    let open CCResult in
    let* languages =
      CCList.filter_map
        (fun (k, v) ->
          match CCList.hd v with
          | "true" -> Some k
          | _ -> None)
        languages
      |> Settings.TenantLanguages.create
    in
    Ok [ Settings.LanguagesUpdated languages |> Pool_event.settings ]
  ;;

  let can = Utils.todo
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
      CCList.map (fun (_, v) -> CCList.hd v) suffixes
      |> Settings.TenantEmailSuffixes.create
    in
    Ok [ Settings.EmailSuffixesUpdated suffixes |> Pool_event.settings ]
  ;;

  let can = Utils.todo
end
