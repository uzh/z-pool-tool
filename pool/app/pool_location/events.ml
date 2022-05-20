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
  | Created of t * Mapping.Write.file list
  | FilesUploaded of t * Mapping.Write.file list
  | Updated of t * update
  | FileDeleted of Mapping.file
[@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t = function
  | Created (({ id; _ } as location), files) ->
    let open Entity.Mapping.Write in
    let%lwt () =
      files
      |> CCList.map (fun { label; language; asset_id; _ } ->
             create label language asset_id id)
      |> Repo.insert pool location
    in
    Lwt.return_unit
  | FilesUploaded ({ id; _ }, files) ->
    let open Entity.Mapping.Write in
    let%lwt () =
      files
      |> CCList.map (fun { label; language; asset_id; _ } ->
             create label language asset_id id)
      |> Repo.RepoFileMapping.insert_multiple pool
    in
    Lwt.return_unit
  | Updated (location, m) ->
    let%lwt () =
      { location with
        name = m.name
      ; description = m.description
      ; address = m.address
      ; link = m.link
      ; status = m.status
      }
      |> Repo.update pool
    in
    Lwt.return_unit
  | FileDeleted file ->
    let%lwt () = Repo_file_mapping.delete pool file in
    Lwt.return_unit
;;
