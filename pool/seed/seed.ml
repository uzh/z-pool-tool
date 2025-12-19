module Assets = Seed_assets

module Root = struct
  let create () =
    let%lwt () = Seed_tenant.create () in
    let%lwt () = Seed_guard.create Database.Pool.Root.label in
    let%lwt () = Seed_smtp.create Database.Pool.Root.label in
    Lwt.return_unit
  ;;
end

module Tenant = struct
  let create ?(is_test = false) db_pools =
    Lwt_list.iter_s
      (fun pool ->
         let seeds =
           (if is_test
            then []
            else
              [ Seed_user.contacts; Seed_guard.create_role_assignments; Seed_smtp.create ])
           @ [ Seed_experiment.experiments
             ; Seed_custom_fields.create
             ; Seed_user.admins
             ; Seed_location.create
             ; Seed_session.create
             ; Seed_timewindow.timewindow
             ; Seed_invitation.invitations
             ; Seed_waiting_list.waiting_list
             ; Seed_assignment.assignment
             ; Seed_mailings.create
             ; Seed_filter.filter
             ; Seed_organisational_units.create
             ; Seed_guard.create
             ; Seed_gtx_api_key.create
             ]
         in
         seeds |> Lwt_list.iter_s (fun fnc -> fnc pool))
      db_pools
  ;;

  let create_contacts db_label () = Seed_user.contacts db_label
end

module DemoInstance = struct
  (** Seed for Demo Instances

      Demo Instances have restricted Permissions on the following features:
      - Tenant System Settings (to restrict changes to Page Scripts due to Security concerns)
      - SMTP Configuration (to ensure only preconfigured SMTP (Mailtrap) is used)
      - API Keys (to avoid creation of API Keys and prevent automated traffic)
      - Permissions (to ensure Permission Restrictions applied by this Seed cannot be change by the user)
  *)

  let src = Logs.Src.create "database.seed.demo"

  module RestrictedPermissions = struct
    open Guard

    (** Permissions that allow creating / modifying Page Scripts *)
    let system_setting_permissions : RolePermission.t list =
      let open Permission in
      [ RolePermission.create `Recruiter Update `SystemSetting
      ; RolePermission.create `Operator Manage `SystemSetting
      ]
    ;;

    (** Permissions that allow changing SMTP configuration *)
    let smtp_permissions : RolePermission.t list =
      let open Permission in
      [ RolePermission.create `Operator Create `Smtp
      ; RolePermission.create `Operator Update `Smtp
      ; RolePermission.create `Operator Delete `Smtp
      ; RolePermission.create `Operator Manage `Smtp
      ]
    ;;

    (** Permissions related to API keys *)
    let api_key_permissions : RolePermission.t list =
      let open Permission in
      [ RolePermission.create `Operator Create `ApiKey
      ; RolePermission.create `Operator Manage `ApiKey
      ]
    ;;

    (** Permissions that allow changing Permissions *)
    let permission_permissions : RolePermission.t list =
      let open Permission in
      [ RolePermission.create `Operator Update `Permission
      ; RolePermission.create `Operator Delete `Permission
      ; RolePermission.create `Operator Manage `Permission
      ]
    ;;

    (** All permissions to remove for demo mode *)
    let all =
      system_setting_permissions
      @ smtp_permissions
      @ api_key_permissions
      @ permission_permissions
    ;;
  end

  (** Remove a single role permission from the database *)
  let remove_permission pool permission =
    let tags = Database.Logger.Tags.create pool in
    Logs.debug ~src (fun m ->
      m ~tags "Removing permission: %s" ([%show: Guard.RolePermission.t] permission));
    Guard.RolePermissionDeleted permission |> Guard.handle_event pool
  ;;

  (** Remove all demo-restricted permissions from the database *)
  let remove_restricted_permissions pool =
    let tags = Database.Logger.Tags.create pool in
    Logs.info ~src (fun m -> m ~tags "Removing demo-restricted permissions");
    Lwt_list.iter_s (remove_permission pool) RestrictedPermissions.all
  ;;

  (** Initialize demo instance by removing restricted permissions and seeding demo data *)
  let create pool =
    let tags = Database.Logger.Tags.create pool in
    Logs.info ~src (fun m -> m ~tags "Initializing demo instance");
    (* Remove permissions for restricted features *)
    let%lwt () = remove_restricted_permissions pool in
    Logs.info ~src (fun m -> m ~tags "Demo instance initialization complete");
    Lwt.return_unit
  ;;
end
