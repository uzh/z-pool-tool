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
  ; disabled : Disabled.t
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
  | Destroyed of t
  | Disabled of t
  | Enabled of t
  | ActivateMaintenance of t
  | DeactivateMaintenance of t
  | OperatorAssigned of t * Sihl_user.t
  | OperatorDivested of t * Sihl_user.t
  | StatusReportGenerated of unit

let handle_event : event -> unit Lwt.t = function
  | Added create_t ->
    create
      create_t.title
      create_t.description
      create_t.url
      create_t.database
      create_t.smtp_auth
      create_t.styles
      create_t.icon
      create_t.logos
      create_t.partner_logos
      create_t.disabled
      create_t.default_language
      ()
    |> Repo.insert
  | Edited (tenant, update_t) ->
    let title = update_t.title in
    let description = update_t.description in
    let url = update_t.url in
    let database = update_t.database in
    let smtp_auth = update_t.smtp_auth in
    let styles = update_t.styles in
    let icon = update_t.icon in
    let logos = update_t.logos in
    let partner_logos = update_t.partner_logos in
    let disabled = update_t.disabled in
    let default_language = update_t.default_language in
    let updated_at = Ptime_clock.now () in
    { tenant with
      title
    ; description
    ; url
    ; database
    ; smtp_auth
    ; styles
    ; icon
    ; logos
    ; partner_logos
    ; disabled
    ; default_language
    ; updated_at
    }
    |> Repo.update
  | Destroyed tenant -> Repo.destroy (tenant.id |> Id.to_human)
  | Disabled tenant ->
    let disabled = true in
    { tenant with disabled } |> Repo.update
  | Enabled tenant ->
    let disabled = false in
    { tenant with disabled } |> Repo.update
  | ActivateMaintenance tenant ->
    let maintenance = true in
    { tenant with maintenance } |> Repo.update
  | DeactivateMaintenance tenant ->
    let maintenance = false in
    { tenant with maintenance } |> Repo.update
  | OperatorAssigned (tenant, user) ->
    Permission.assign user (Role.operator (tenant.id |> Id.to_human))
  | OperatorDivested (tenant, user) ->
    Permission.divest user (Role.operator (tenant.id |> Id.to_human))
  | StatusReportGenerated _ -> Utils.todo ()
;;

let equal_event event1 event2 =
  match event1, event2 with
  | Added one, Added two -> equal_create one two
  | Edited (tenant_one, update_one), Edited (tenant_two, update_two) ->
    equal tenant_one tenant_two && equal_update update_one update_two
  | Destroyed one, Destroyed two
  | Enabled one, Enabled two
  | Disabled one, Disabled two
  | ActivateMaintenance one, ActivateMaintenance two
  | DeactivateMaintenance one, DeactivateMaintenance two -> equal one two
  | ( OperatorAssigned (tenant_one, user_one)
    , OperatorAssigned (tenant_two, user_two) )
  | ( OperatorDivested (tenant_one, user_one)
    , OperatorDivested (tenant_two, user_two) ) ->
    equal tenant_one tenant_two
    && String.equal user_one.Sihl_user.id user_two.Sihl_user.id
  | _ -> false
;;

let pp_event formatter event =
  match event with
  | Added m -> pp_create formatter m
  | Edited (tenant, update) ->
    let () = pp formatter tenant in
    pp_update formatter update
  | Destroyed m
  | Disabled m
  | Enabled m
  | ActivateMaintenance m
  | DeactivateMaintenance m -> pp formatter m
  | OperatorAssigned (tenant, user) | OperatorDivested (tenant, user) ->
    let () = pp formatter tenant in
    Sihl_user.pp formatter user
  | StatusReportGenerated () -> Utils.todo ()
;;
