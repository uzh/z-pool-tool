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
  | FileUploaded of Mapping.Write.file
  | Updated of t * update * Pool_common.Id.t
  | FileDeleted of Mapping.Id.t
[@@deriving eq, show, variants]

let handle_event pool : event -> unit Lwt.t =
  let open Utils.Lwt_result.Infix in
  let ctx = Database.to_ctx pool in
  function
  | Created ({ files; _ } as location) ->
    let%lwt () =
      files
      |> CCList.map (Repo.RepoFileMapping.of_entity location)
      |> Repo.insert pool location
    in
    Entity_guard.Target.to_authorizable ~ctx location
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : Guard.Target.t) -> ()
  | FileUploaded file ->
    let open Entity.Mapping.Write in
    let%lwt () =
      file
      |> (fun { label; language; asset_id; location_id; _ } ->
           create label language asset_id location_id)
      |> Repo.RepoFileMapping.insert pool
    in
    Entity_guard.FileTarget.to_authorizable_of_write
      ~ctx:(Database.to_ctx pool)
      file
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : Guard.Target.t) -> ()
  | Updated (location, m, user_uuid) ->
    let updated =
      { location with
        name = m.name
      ; description = m.description
      ; address = m.address
      ; link = m.link
      ; status = m.status
      }
    in
    let%lwt () =
      Version_history.create pool ~user_uuid ~before:location ~after:updated ()
    in
    Repo.update pool updated
  | FileDeleted id ->
    let%lwt () = Repo_file_mapping.delete pool id in
    Lwt.return_unit
;;
