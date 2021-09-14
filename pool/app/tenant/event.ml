open Entity

type create =
  { title : Title.t
  ; description : Description.t
  ; url : Url.t
  ; database : Database.t
  ; smtp_auth : SmtpAuth.t
  ; styles : Styles.t
  ; icon : Icon.t
  ; logos : Logos.t
  ; partner_logos : PartnerLogo.t
  ; default_language : Settings.Language.t
  }
[@@deriving eq, show]

type update =
  { title : Title.t
  ; description : Description.t
  ; url : Url.t
  ; database : Database.t
  ; smtp_auth : SmtpAuth.t
  ; styles : Styles.t
  ; icon : Icon.t
  ; logos : Logos.t
  ; partner_logos : PartnerLogo.t
  ; disabled : Disabled.t
  ; default_language : Settings.Language.t
  }
[@@deriving eq, show]

let equal_operator_event (t1, o1) (t2, o2) =
  equal t1 t2 && String.equal o1.Sihl_user.id o2.Sihl_user.id
;;

type event =
  | Added of create [@equal equal]
  | Edited of t * update
  | Destroyed of Common.Id.t
  | Disabled of t
  | Enabled of t
  | ActivateMaintenance of t
  | DeactivateMaintenance of t
  | OperatorAssigned of t * Admin.operator Admin.t
  | OperatorDivested of t * Admin.operator Admin.t
  | StatusReportGenerated of unit

let handle_event : event -> unit Lwt.t = function
  | Added m ->
    create
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
  | Edited (tenant, update_t) ->
    { tenant with
      title = update_t.title
    ; description = update_t.description
    ; url = update_t.url
    ; database = update_t.database
    ; smtp_auth = update_t.smtp_auth
    ; styles = update_t.styles
    ; icon = update_t.icon
    ; logos = update_t.logos
    ; partner_logos = update_t.partner_logos
    ; disabled = update_t.disabled
    ; default_language = update_t.default_language
    ; updated_at = Ptime_clock.now ()
    }
    |> Repo.update
  | Destroyed tenant_id -> Repo.destroy tenant_id
  | Disabled tenant ->
    let disabled = true |> Disabled.create in
    { tenant with disabled } |> Repo.update
  | Enabled tenant ->
    let disabled = false |> Disabled.create in
    { tenant with disabled } |> Repo.update
  | ActivateMaintenance tenant ->
    let maintenance = true |> Maintenance.create in
    { tenant with maintenance } |> Repo.update
  | DeactivateMaintenance tenant ->
    let maintenance = false |> Maintenance.create in
    { tenant with maintenance } |> Repo.update
  | OperatorAssigned (tenant, user) ->
    Permission.assign (Admin.user user) (Role.operator tenant.id)
  | OperatorDivested (tenant, user) ->
    Permission.divest (Admin.user user) (Role.operator tenant.id)
  | StatusReportGenerated _ -> Utils.todo ()
;;

let equal_event event1 event2 =
  match event1, event2 with
  | Added one, Added two -> equal_create one two
  | Edited (tenant_one, update_one), Edited (tenant_two, update_two) ->
    equal tenant_one tenant_two && equal_update update_one update_two
  | Destroyed one, Destroyed two ->
    String.equal (one |> Common.Id.show) (two |> Common.Id.show)
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
  | Added m -> pp_create formatter m
  | Edited (tenant, update) ->
    let () = pp formatter tenant in
    pp_update formatter update
  | Destroyed m -> Common.Id.pp formatter m
  | Disabled m | Enabled m | ActivateMaintenance m | DeactivateMaintenance m ->
    pp formatter m
  | OperatorAssigned (tenant, user) | OperatorDivested (tenant, user) ->
    let () = pp formatter tenant in
    Admin.pp formatter user
  | StatusReportGenerated () -> Utils.todo ()
;;
