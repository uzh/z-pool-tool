open Entity
module Id = Pool_common.Id

let get_or_failwith = Pool_common.Utils.get_or_failwith

type update =
  { title : Title.t
  ; description : Description.t option
  ; url : Url.t
  ; disabled : Disabled.t
  ; default_language : Pool_common.Language.t
  ; styles : Styles.Write.t option
  ; icon : Icon.Write.t option
  }
[@@deriving eq, show]

type logo_mappings = LogoMapping.Write.t list [@@deriving eq, show]

type event =
  | Created of (Write.t * Database.t)
  | LogosUploaded of logo_mappings
  | LogoDeleted of t * Id.t
  | DetailsEdited of Write.t * update
  | DatabaseEdited of Write.t * Database.t
  | Destroyed of Id.t
  | ActivateMaintenance of Write.t
  | DeactivateMaintenance of Write.t
  | GtxApiKeyUpdated of Write.t * GtxApiKey.t
  | GtxApiKeyRemoved of Write.t
[@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t = function
  | Created (({ Write.id; _ } as tenant), database) ->
    let open Utils.Lwt_result.Infix in
    let open Guard in
    let ctx = Database.to_ctx pool in
    let%lwt () = Repo.insert Database.root (tenant, database) in
    let%lwt () =
      Repo.find pool id
      >>= Entity_guard.Target.to_authorizable ~ctx
      ||> get_or_failwith
      ||> fun (_ : Target.t) -> ()
    in
    Lwt.return_unit
  | LogosUploaded logo_mappings ->
    let%lwt _ = Repo.LogoMappingRepo.insert_multiple logo_mappings in
    Lwt.return_unit
  | LogoDeleted (tenant, asset_id) ->
    let%lwt () = Repo.LogoMappingRepo.delete tenant.id asset_id in
    Lwt.return_unit
  | DetailsEdited (tenant, update_t) ->
    let open Entity.Write in
    let open CCOption.Infix in
    { tenant with
      title = update_t.title
    ; description = update_t.description
    ; url = update_t.url
    ; styles = update_t.styles <+> tenant.styles
    ; icon = update_t.icon <+> tenant.icon
    ; disabled = update_t.disabled
    ; default_language = update_t.default_language
    ; updated_at = Ptime_clock.now ()
    }
    |> Repo.update Database.root
  | DatabaseEdited (tenant, database) ->
    let%lwt () = Repo.update_database Database.root (tenant, database) in
    Lwt.return_unit
  | Destroyed tenant_id -> Repo.destroy tenant_id
  | ActivateMaintenance tenant ->
    let open Entity.Write in
    let maintenance = true |> Maintenance.create in
    let%lwt () = { tenant with maintenance } |> Repo.update Database.root in
    Lwt.return_unit
  | DeactivateMaintenance tenant ->
    let open Entity.Write in
    let maintenance = false |> Maintenance.create in
    let%lwt () = { tenant with maintenance } |> Repo.update Database.root in
    Lwt.return_unit
  | GtxApiKeyUpdated (tenant, gtx_api_key) ->
    let open Entity.Write in
    { tenant with gtx_api_key = Some gtx_api_key } |> Repo.update Database.root
  | GtxApiKeyRemoved tenant ->
    let open Entity.Write in
    { tenant with gtx_api_key = None } |> Repo.update Database.root
;;
