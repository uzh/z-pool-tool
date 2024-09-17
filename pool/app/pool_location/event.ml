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
  | Updated of t * update
  | FileDeleted of Mapping.Id.t
[@@deriving eq, show, variants]

let create_changelog pool ?user_uuid before after =
  let open Version_history in
  user_uuid
  |> CCOption.map_or ~default:Lwt.return_unit (fun user_uuid ->
    insert
      pool
      ~entity_uuid:(Entity.Id.to_common before.id)
      ~user_uuid
      ~before
      ~after
      ())
;;

let handle_event ?user_uuid pool : event -> unit Lwt.t =
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
    let%lwt () = create_changelog pool ?user_uuid location updated in
    updated |> Repo.update pool
  | FileDeleted id ->
    let%lwt () = Repo_file_mapping.delete pool id in
    Lwt.return_unit
;;
