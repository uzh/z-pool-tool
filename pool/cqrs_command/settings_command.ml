module UpdateLanguages : sig
  type t = (string * string list) list

  val handle : t -> (Pool_event.t list, string) Result.t
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = (string * string list) list

  let handle languages =
    let open CCResult in
    let* langauges =
      CCList.filter_map
        (fun (k, v) ->
          match CCList.hd v with
          | "true" -> Some k
          | _ -> None)
        languages
      |> Settings.TenantLanguages.create
    in
    Ok [ Settings.LanguagesUpdated langauges |> Pool_event.settings ]
  ;;

  let can = Utils.todo
end
