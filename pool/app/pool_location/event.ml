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

let handle_event pool : event -> unit Lwt.t =
  let open Utils.Lwt_result.Infix in
  let ctx = Pool_database.to_ctx pool in
  function
  | Created ({ id; files; _ } as location) ->
    let%lwt () =
      files
      |> CCList.map (Repo.RepoFileMapping.of_entity location)
      |> Repo.insert pool location
    in
    let%lwt (_ : Guard.Rule.t list) =
      let open Guard.Persistence in
      Admin.Guard.RuleSet.location_manager id
      |> Rule.save_all ~ctx
      >|- (fun err ->
            Pool_common.Message.nothandled
            @@ Format.asprintf
                 "Failed to save: %s"
                 ([%show: Guard.Rule.t list] err))
      ||> CCFun.tap (fun _ -> Cache.clear ())
      ||> Pool_common.Utils.get_or_failwith
    in
    Entity_guard.Target.to_authorizable ~ctx location
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : Role.Target.t Guard.Target.t) -> ()
  | FileUploaded file ->
    let open Entity.Mapping.Write in
    let%lwt () =
      file
      |> (fun { label; language; asset_id; location_id; _ } ->
           create label language asset_id location_id)
      |> Repo.RepoFileMapping.insert pool
    in
    Entity_guard.FileTarget.to_authorizable_of_write
      ~ctx:(Pool_database.to_ctx pool)
      file
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : Role.Target.t Guard.Target.t) -> ()
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
