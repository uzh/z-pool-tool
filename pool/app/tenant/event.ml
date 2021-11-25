open Entity
module Id = Pool_common.Id
module Database = Pool_common.Database

type smtp_auth_update =
  { server : SmtpAuth.Server.t
  ; port : SmtpAuth.Port.t
  ; username : SmtpAuth.Username.t
  ; authentication_method : SmtpAuth.AuthenticationMethod.t
  ; protocol : SmtpAuth.Protocol.t
  }
[@@deriving eq, show]

type update =
  { title : Title.t
  ; description : Description.t
  ; url : Pool_common.Url.t
  ; smtp_auth : smtp_auth_update
  ; disabled : Disabled.t
  ; default_language : Pool_common.Language.t
  }
[@@deriving eq, show]

type logo_mappings = LogoMapping.Write.t list [@@deriving eq, show]

let equal_operator_event (t1, o1) (t2, o2) =
  equal t1 t2 && CCString.equal o1.Sihl_user.id o2.Sihl_user.id
;;

type event =
  | Created of Write.t
  | LogosUploaded of logo_mappings
  | LogoDeleted of t * Id.t
  | DetailsEdited of Write.t * update
  | DatabaseEdited of Write.t * Database.t
  | Destroyed of Id.t
  | ActivateMaintenance of Write.t
  | DeactivateMaintenance of Write.t
  | OperatorAssigned of Id.t * Admin.operator Admin.t
  | OperatorDivested of Id.t * Admin.operator Admin.t
  | StatusReportGenerated of unit

let handle_event _ : event -> unit Lwt.t = function
  | Created tenant ->
    let%lwt () = Repo.insert Database.root tenant in
    Lwt.return_unit
  | LogosUploaded logo_mappings ->
    let%lwt _ = Repo.LogoMappingRepo.insert_multiple logo_mappings in
    Lwt.return_unit
  | LogoDeleted (tenant, asset_id) ->
    let%lwt () = Repo.LogoMappingRepo.delete tenant.id asset_id in
    Lwt.return_unit
  | DetailsEdited (tenant, update_t) ->
    let open Entity.Write in
    let smtp_auth =
      SmtpAuth.Write.
        { tenant.smtp_auth with
          server = update_t.smtp_auth.server
        ; port = update_t.smtp_auth.port
        ; username = update_t.smtp_auth.username
        ; protocol = update_t.smtp_auth.protocol
        }
    in
    let%lwt () =
      let tenant =
        { tenant with
          title = update_t.title
        ; description = update_t.description
        ; url = update_t.url
        ; smtp_auth
        ; disabled = update_t.disabled
        ; default_language = update_t.default_language
        ; updated_at = Ptime_clock.now ()
        }
      in
      Repo.update Database.root tenant
    in
    Lwt.return_unit
  | DatabaseEdited (tenant, database) ->
    let open Entity.Write in
    let%lwt () =
      { tenant with database; updated_at = Ptime_clock.now () }
      |> Repo.update Database.root
    in
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
  | OperatorAssigned (tenant_id, user) ->
    Permission.assign (Admin.user user) (Role.operator tenant_id)
  | OperatorDivested (tenant_id, user) ->
    Permission.divest (Admin.user user) (Role.operator tenant_id)
  | StatusReportGenerated _ -> Utils.todo ()
;;

let[@warning "-4"] equal_event event1 event2 =
  match event1, event2 with
  | Created tenant_one, Created tenant_two -> Write.equal tenant_one tenant_two
  | LogosUploaded logo_mappings_one, LogosUploaded logo_mappings_two ->
    equal_logo_mappings logo_mappings_one logo_mappings_two
  | LogoDeleted (tenant_one, id_one), LogoDeleted (tenant_two, id_two) ->
    equal tenant_one tenant_two
    && CCString.equal (Id.value id_one) (Id.value id_two)
  | ( DetailsEdited (tenant_one, update_one)
    , DetailsEdited (tenant_two, update_two) ) ->
    Write.equal tenant_one tenant_two && equal_update update_one update_two
  | ( DatabaseEdited (tenant_one, database_one)
    , DatabaseEdited (tenant_two, database_two) ) ->
    Write.equal tenant_one tenant_two
    && Database.equal database_one database_two
  | Destroyed one, Destroyed two -> Id.equal one two
  | ActivateMaintenance one, ActivateMaintenance two
  | DeactivateMaintenance one, DeactivateMaintenance two -> Write.equal one two
  | ( OperatorAssigned (tenant_id_one, user_one)
    , OperatorAssigned (tenant_id_two, user_two) )
  | ( OperatorDivested (tenant_id_one, user_one)
    , OperatorDivested (tenant_id_two, user_two) ) ->
    CCString.equal (tenant_id_one |> Id.value) (tenant_id_two |> Id.value)
    && CCString.equal
         (Admin.user user_one).Sihl_user.id
         (Admin.user user_two).Sihl_user.id
  | _ -> false
;;

let pp_event formatter event =
  match event with
  | Created tenant -> Write.pp formatter tenant
  | LogosUploaded logo_mappings -> pp_logo_mappings formatter logo_mappings
  | LogoDeleted (tenant, id) ->
    pp formatter tenant;
    Id.pp formatter id
  | DetailsEdited (tenant, update) ->
    Write.pp formatter tenant;
    pp_update formatter update
  | DatabaseEdited (tenant, database) ->
    Write.pp formatter tenant;
    Database.pp formatter database
  | Destroyed m -> Id.pp formatter m
  | ActivateMaintenance m | DeactivateMaintenance m -> Write.pp formatter m
  | OperatorAssigned (tenant_id, user) | OperatorDivested (tenant_id, user) ->
    Id.pp formatter tenant_id;
    Admin.pp formatter user
  | StatusReportGenerated () -> Utils.todo ()
;;
