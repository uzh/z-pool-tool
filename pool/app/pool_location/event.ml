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

let handle_event pool : event -> unit Lwt.t = function
  | Created ({ files; _ } as location) ->
    let%lwt () =
      files
      |> CCList.map (Repo.RepoFileMapping.of_entity location)
      |> Repo.insert pool location
    in
    Lwt.return_unit
  | FileUploaded file ->
    let open Entity.Mapping.Write in
    let%lwt () =
      file
      |> (fun { label; language; asset_id; location_id; _ } ->
           create label language asset_id location_id)
      |> Repo.RepoFileMapping.insert pool
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
  | FileDeleted id ->
    let%lwt () = Repo_file_mapping.delete pool id in
    Lwt.return_unit
;;
