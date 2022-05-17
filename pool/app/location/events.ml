open Entity

type update =
  { name : Name.t
  ; description : Description.t
  ; address : MailingAddress.t
  ; link : Link.t
  ; status : Status.t
  }
[@@deriving eq, show]

type event =
  | Created of t
  | FilesUploaded of t * Mapping.file list
  | Updated of t * update
[@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t = function
  | Created location ->
    let%lwt () = Repo.insert pool location in
    Lwt.return_unit
  | FilesUploaded (location, files) ->
    (* TODO files to write files *)
    let%lwt _ =
      files
      |> CCList.map (Repo.RepoFileMapping.of_entity location)
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
;;
