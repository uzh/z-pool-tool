open Entity

type update =
  { name : Name.t
  ; description : Description.t option
  ; address : Address.t
  ; link : Link.t option
  ; status : Status.t
  }
[@@deriving eq, show]

type event =
  | Created of t
  | FileUploaded of File.Write.file
  | Updated of t * update
  | FileDeleted of File.Id.t
[@@deriving eq, show, variants]

let handle_event ?user_uuid pool : event -> unit Lwt.t =
  let open Utils.Lwt_result.Infix in
  let ctx = Database.to_ctx pool in
  let create_changelog before after =
    let open Version_history in
    insert pool ?user_uuid ~entity_uuid:before.id ~before ~after ()
  in
  function
  | Created location ->
    let%lwt () = Repo.insert pool location in
    Entity_guard.Target.to_authorizable ~ctx location
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : Guard.Target.t) -> ()
  | FileUploaded file ->
    let open Entity.File.Write in
    let%lwt () =
      file
      |> (fun { label; language; asset_id; location_id; _ } ->
      create label language asset_id location_id)
      |> Repo.RepoFileMapping.insert pool
    in
    Entity_guard.FileTarget.to_authorizable_of_write ~ctx:(Database.to_ctx pool) file
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : Guard.Target.t) -> ()
  | Updated (location, m) ->
    let updated =
      { location with
        name = m.name
      ; description = m.description
      ; address = m.address
      ; link = m.link
      ; status = m.status
      }
    in
    let%lwt () = create_changelog location updated in
    updated |> Repo.update pool
  | FileDeleted id ->
    let%lwt () = Repo_file_mapping.delete pool id in
    Lwt.return_unit
;;
