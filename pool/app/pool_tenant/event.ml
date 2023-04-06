open Entity
module Id = Pool_common.Id
module Database = Pool_database

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
  | Created of Write.t
  | LogosUploaded of logo_mappings
  | LogoDeleted of t * Id.t
  | DetailsEdited of Write.t * update
  | DatabaseEdited of Write.t * Database.t
  | SmtpCreated of SmtpAuth.Write.t
  | SmtpEdited of SmtpAuth.t
  | SmtpPasswordEdited of SmtpAuth.update_password
  | Destroyed of Id.t
  | ActivateMaintenance of Write.t
  | DeactivateMaintenance of Write.t
[@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t = function
  | Created ({ Write.id; _ } as tenant) ->
    let open Utils.Lwt_result.Infix in
    let open Guard in
    let ctx = Pool_database.to_ctx pool in
    let%lwt () = Repo.insert Database.root tenant in
    let%lwt () =
      Repo.find pool id
      >>= Entity_guard.Target.to_authorizable ~ctx
      ||> get_or_failwith
      ||> fun (_ : Role.Target.t Target.t) -> ()
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
    let%lwt () =
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
    in
    Lwt.return_unit
  | DatabaseEdited (tenant, database) ->
    let open Entity.Write in
    let%lwt () =
      { tenant with database; updated_at = Ptime_clock.now () }
      |> Repo.update Database.root
    in
    Lwt.return_unit
  | SmtpCreated ({ SmtpAuth.Write.id; _ } as created) ->
    let open Utils.Lwt_result.Infix in
    let ctx = Pool_database.to_ctx pool in
    let%lwt () = Repo.Smtp.insert pool created in
    let%lwt () =
      Repo.Smtp.find pool id
      >>= Entity_guard.SmtpTarget.to_authorizable ~ctx
      ||> get_or_failwith
      ||> fun (_ : Role.Target.t Guard.Target.t) -> ()
    in
    let () = Pool_tenant_service.Email.remove_from_cache pool in
    Lwt.return_unit
  | SmtpEdited updated ->
    let%lwt () = Repo.Smtp.update pool updated in
    let () = Pool_tenant_service.Email.remove_from_cache pool in
    Lwt.return_unit
  | SmtpPasswordEdited updated_password ->
    let%lwt () = Repo.Smtp.update_password pool updated_password in
    let () = Pool_tenant_service.Email.remove_from_cache pool in
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
;;
