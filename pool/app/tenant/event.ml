open Entity
module Id = Pool_common.Id

type create =
  { title : Title.t
  ; description : Description.t
  ; url : Url.t
  ; database : Database.t
  ; smtp_auth : SmtpAuth.t
  ; styles : Styles.t
  ; icon : Icon.t
  ; logos : Logos.t
  ; partner_logos : PartnerLogos.t
  ; default_language : Settings.Language.t
  }
[@@deriving eq, show]

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
  ; url : Url.t
  ; smtp_auth : smtp_auth_update
  ; styles : Styles.t
  ; icon : Icon.t
  ; logos : Logos.t
  ; partner_logos : PartnerLogos.t
  ; disabled : Disabled.t
  ; default_language : Settings.Language.t
  }
[@@deriving eq, show]

let equal_operator_event (t1, o1) (t2, o2) =
  equal t1 t2 && String.equal o1.Sihl_user.id o2.Sihl_user.id
;;

type event =
  | Created of create [@equal equal]
  | DetailsEdited of t * update
  | DatabaseEdited of t * Database.t
  | Destroyed of Id.t
  | Disabled of t
  | Enabled of t
  | ActivateMaintenance of t
  | DeactivateMaintenance of t
  | OperatorAssigned of t * Admin.operator Admin.t
  | OperatorDivested of t * Admin.operator Admin.t
  | StatusReportGenerated of unit

let handle_event : event -> unit Lwt.t =
  let open Lwt.Syntax in
  function
  | Created m ->
    let* _ =
      Entity.create
        m.title
        m.description
        m.url
        m.database
        m.smtp_auth
        m.styles
        m.icon
        m.logos
        m.partner_logos
        m.default_language
      |> Repo.insert
    in
    Lwt.return_unit
  | DetailsEdited (tenant, update_t) ->
    let smtp_auth =
      SmtpAuth.
        { tenant.smtp_auth with
          server = update_t.smtp_auth.server
        ; port = update_t.smtp_auth.port
        ; username = update_t.smtp_auth.username
        ; protocol = update_t.smtp_auth.protocol
        }
    in
    let* _ =
      { tenant with
        title = update_t.title
      ; description = update_t.description
      ; url = update_t.url
      ; smtp_auth
      ; styles = update_t.styles
      ; icon = update_t.icon
      ; logos = update_t.logos
      ; partner_logos = update_t.partner_logos
      ; disabled = update_t.disabled
      ; default_language = update_t.default_language
      ; updated_at = Ptime_clock.now ()
      }
      |> Repo.update
    in
    Lwt.return_unit
  | DatabaseEdited (tenant, database) ->
    let* _ =
      { tenant with database; updated_at = Ptime_clock.now () } |> Repo.update
    in
    Lwt.return_unit
  | Destroyed tenant_id -> Repo.destroy tenant_id
  (* TODO [timhub]: separate events?? *)
  | Disabled tenant ->
    let disabled = true |> Disabled.create in
    let* _ = { tenant with disabled } |> Repo.update in
    Lwt.return_unit
  | Enabled tenant ->
    let disabled = false |> Disabled.create in
    let* _ = { tenant with disabled } |> Repo.update in
    Lwt.return_unit
  | ActivateMaintenance tenant ->
    let maintenance = true |> Maintenance.create in
    let* _ = { tenant with maintenance } |> Repo.update in
    Lwt.return_unit
  | DeactivateMaintenance tenant ->
    let maintenance = false |> Maintenance.create in
    let* _ = { tenant with maintenance } |> Repo.update in
    Lwt.return_unit
  | OperatorAssigned (tenant, user) ->
    Permission.assign (Admin.user user) (Role.operator tenant.id)
  | OperatorDivested (tenant, user) ->
    Permission.divest (Admin.user user) (Role.operator tenant.id)
  | StatusReportGenerated _ -> Utils.todo ()
;;

let[@warning "-4"] equal_event event1 event2 =
  match event1, event2 with
  | Created one, Created two -> equal_create one two
  | ( DetailsEdited (tenant_one, update_one)
    , DetailsEdited (tenant_two, update_two) ) ->
    equal tenant_one tenant_two && equal_update update_one update_two
  | ( DatabaseEdited (tenant_one, database_one)
    , DatabaseEdited (tenant_two, database_two) ) ->
    equal tenant_one tenant_two && Database.equal database_one database_two
  | Destroyed one, Destroyed two ->
    String.equal (one |> Id.show) (two |> Id.show)
  | Enabled one, Enabled two
  | Disabled one, Disabled two
  | ActivateMaintenance one, ActivateMaintenance two
  | DeactivateMaintenance one, DeactivateMaintenance two -> equal one two
  | ( OperatorAssigned (tenant_one, user_one)
    , OperatorAssigned (tenant_two, user_two) )
  | ( OperatorDivested (tenant_one, user_one)
    , OperatorDivested (tenant_two, user_two) ) ->
    equal tenant_one tenant_two
    && String.equal
         (Admin.user user_one).Sihl_user.id
         (Admin.user user_two).Sihl_user.id
  | _ -> false
;;

let pp_event formatter event =
  match event with
  | Created m -> pp_create formatter m
  | DetailsEdited (tenant, update) ->
    let () = pp formatter tenant in
    pp_update formatter update
  | DatabaseEdited (tenant, database) ->
    let () = pp formatter tenant in
    Database.pp formatter database
  | Destroyed m -> Id.pp formatter m
  | Disabled m | Enabled m | ActivateMaintenance m | DeactivateMaintenance m ->
    pp formatter m
  | OperatorAssigned (tenant, user) | OperatorDivested (tenant, user) ->
    let () = pp formatter tenant in
    Admin.pp formatter user
  | StatusReportGenerated () -> Utils.todo ()
;;
