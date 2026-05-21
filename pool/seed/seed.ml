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

      Demo Instances have restricted Permissions to prevent users from being able to modify the system into an invalid
      state:
      - Tenant System Settings (to restrict changes to Page Scripts due to Security concerns)
      - SMTP Configuration (to ensure only preconfigured SMTP (Mailtrap) is used)
      - API Keys (to avoid creation of API Keys and prevent automated traffic)
      - Permissions (to ensure Permission Restrictions applied by this Seed cannot be changed by the user)

      We use a permission allow list rather than a block list for enforcing permission assignments to prevent
      newly added permissions from automatically being assigned *)

  let src = Logs.Src.create "database.seed.demo"

  module AllowedPermissions = struct
    open Guard

    let map =
      (* the following block list contains the entity types which are potentially dangerous
         for a user to have in a demo environment therefore these entity types
         will all be set to read only *)
      let restricted_entity_types =
        [ `ApiKey; `Permission; `Smtp; `SystemSetting; `System; `RoleOperator ]
      in
      CCList.map (fun (role, permission, model) ->
        let computed_permission =
          CCList.find_opt (Role.Target.equal model) restricted_entity_types
          |> function
          | Some _ -> Permission.Read
          | None -> permission
        in
        RolePermission.create role computed_permission model)
    ;;

    (** see [Guard.operator_permissions false] *)
    let operator_permissions : RolePermission.t list =
      let open Permission in
      [ `Operator, Manage, `Admin
      ; `Operator, Manage, `Announcement
      ; `Operator, Manage, `ApiKey
      ; `Operator, Manage, `Assignment
      ; `Operator, Manage, `Contact
      ; `Operator, Manage, `ContactInfo
      ; `Operator, Manage, `ContactDirectMessage
      ; `Operator, Manage, `ContactName
      ; `Operator, Manage, `CustomField
      ; `Operator, Manage, `CustomFieldGroup
      ; `Operator, Manage, `DuplicateContact
      ; `Operator, Manage, `Experiment
      ; `Operator, Manage, `Filter
      ; `Operator, Manage, `I18n
      ; `Operator, Manage, `Invitation
      ; `Operator, Manage, `InvitationNotification
      ; `Operator, Manage, `Location
      ; `Operator, Manage, `LocationFile
      ; `Operator, Manage, `Mailing
      ; `Operator, Manage, `Message
      ; `Operator, Manage, `MessageTemplate
      ; `Operator, Manage, `OrganisationalUnit
      ; `Operator, Manage, `Permission
      ; `Operator, Manage, `Queue
      ; `Operator, Manage, `Role
      ; `Operator, Manage, `Schedule
      ; `Operator, Manage, `Session
      ; `Operator, Manage, `SessionClose
      ; `Operator, Manage, `SignupCode
      ; `Operator, Manage, `Smtp
      ; `Operator, Manage, `Statistics
      ; `Operator, Manage, `System
      ; `Operator, Manage, `SystemSetting
      ; `Operator, Manage, `Tag
      ; `Operator, Manage, `Tenant
      ; `Operator, Manage, `Version
      ; `Operator, Manage, `WaitingList
      ]
      |> map
    ;;

    (** see [Guard.operator_permissions true] *)
    let operator_role_permissions : RolePermission.t list =
      let open Permission in
      [ `Operator, Manage, `RoleAssistant
      ; `Operator, Manage, `RoleExperimenter
      ; `Operator, Manage, `RoleLocationManager
      ; `Operator, Manage, `RoleOperator
      ; `Operator, Manage, `RoleRecruiter
      ]
      |> map
    ;;

    (** see [Guard.recruiter_permissions] *)
    let recruiter_permissions : RolePermission.t list =
      let open Permission in
      [ `Recruiter, Create, `Admin
      ; `Recruiter, Read, `Admin
      ; `Recruiter, Update, `Admin
      ; `Recruiter, Manage, `Assignment
      ; `Recruiter, Manage, `Contact
      ; `Recruiter, Manage, `CustomField
      ; `Recruiter, Manage, `CustomFieldGroup
      ; `Recruiter, Manage, `Experiment
      ; `Recruiter, Manage, `Filter
      ; `Recruiter, Manage, `I18n
      ; `Recruiter, Manage, `Invitation
      ; `Recruiter, Manage, `Location
      ; `Recruiter, Manage, `LocationFile
      ; `Recruiter, Manage, `Mailing
      ; `Recruiter, Manage, `MessageTemplate
      ; `Recruiter, Read, `Permission
      ; `Recruiter, Read, `OrganisationalUnit
      ; `Recruiter, Create, `Queue
      ; `Recruiter, Read, `Queue
      ; `Recruiter, Manage, `Role
      ; `Recruiter, Read, `Schedule
      ; `Recruiter, Manage, `Session
      ; `Recruiter, Read, `SystemSetting
      ; `Recruiter, Read, `Smtp
      ; `Recruiter, Read, `Statistics
      ; `Recruiter, Read, `System
      ; `Recruiter, Manage, `Tag
      ; `Recruiter, Read, `Tenant
      ; `Recruiter, Manage, `WaitingList
      ]
      |> map
    ;;

    (** see [Guard.recruiter_role_permissions] *)
    let recruiter_role_permissions : RolePermission.t list =
      let open Permission in
      [ `Recruiter, Manage, `ContactDirectMessage
      ; `Recruiter, Manage, `InvitationNotification
      ; `Recruiter, Manage, `RoleAdmin
      ; `Recruiter, Manage, `RoleAssistant
      ; `Recruiter, Manage, `RoleExperimenter
      ; `Recruiter, Manage, `RoleLocationManager
      ; `Recruiter, Read, `RoleRecruiter
      ; `Recruiter, Create, `RoleRecruiter
      ; `Recruiter, Read, `RoleOperator
      ]
      |> map
    ;;

    (** see [Guard.assistant_permissions] *)
    let assistant_permissions : RolePermission.t list =
      let open Permission in
      [ `Assistant, Read, `Contact
      ; `Assistant, Update, `Contact
      ; `Assistant, Create, `Message
      ; `Assistant, Read, `Session
      ; `Assistant, Read, `SessionClose
      ; `Assistant, Update, `SessionClose
      ; `Assistant, Read, `Assignment
      ; `Assistant, Update, `Assignment
      ; `Assistant, Read, `Experiment
      ; `Assistant, Read, `WaitingList
      ; `Assistant, Update, `WaitingList
      ]
      |> map
    ;;

    (** see [Guard.assistant_role_permissions] *)
    let assistant_role_permissions : RolePermission.t list =
      let open Permission in
      [ `Assistant, Read, `RoleAdmin
      ; `Assistant, Create, `RoleAdmin
      ; `Assistant, Read, `RoleAssistant
      ; `Assistant, Create, `RoleAssistant
      ; `Assistant, Read, `RoleExperimenter
      ; `Assistant, Read, `RoleLocationManager
      ; `Assistant, Read, `RoleRecruiter
      ]
      |> map
    ;;

    (** see [Guard.experimenter_permissions] *)
    let experimenter_permissions : RolePermission.t list =
      let open Permission in
      [ `Experimenter, Read, `ContactName
      ; `Experimenter, Read, `Session
      ; `Experimenter, Read, `SessionClose
      ; `Experimenter, Update, `SessionClose
      ; `Experimenter, Read, `Assignment
      ; `Experimenter, Update, `Assignment
      ; `Experimenter, Read, `Experiment
      ; `Experimenter, Read, `RoleExperimenter
      ]
      |> map
    ;;

    (** see [Guard.experimenter_role_permissions] *)
    let experimenter_role_permissions : RolePermission.t list =
      let open Permission in
      [ `Experimenter, Read, `RoleAdmin
      ; `Experimenter, Read, `RoleAssistant
      ; `Experimenter, Read, `RoleExperimenter
      ; `Experimenter, Read, `RoleLocationManager
      ; `Experimenter, Read, `RoleRecruiter
      ]
      |> map
    ;;

    (** see [Guard.location_manager_permissions] *)
    let location_manager_permissions : RolePermission.t list =
      let open Permission in
      [ `LocationManager, Manage, `Location
      ; `LocationManager, Manage, `LocationFile
      ; `LocationManager, Read, `ContactName
      ; `LocationManager, Read, `ContactInfo
      ; `LocationManager, Read, `Session
      ]
      |> map
    ;;

    (** see [Guard.location_manager_role_permissions] *)
    let location_manager_role_permissions : RolePermission.t list =
      let open Permission in
      [ `LocationManager, Read, `RoleLocationManager
      ; `LocationManager, Create, `RoleLocationManager
      ; `LocationManager, Read, `RoleAssistant
      ; `LocationManager, Read, `RoleExperimenter
      ; `LocationManager, Read, `RoleRecruiter
      ]
      |> map
    ;;

    let all =
      operator_permissions
      @ operator_role_permissions
      @ recruiter_permissions
      @ recruiter_role_permissions
      @ assistant_permissions
      @ assistant_role_permissions
      @ experimenter_permissions
      @ experimenter_role_permissions
      @ location_manager_permissions
      @ location_manager_role_permissions
    ;;
  end

  let remove_all_permissions pool =
    let tags = Database.Logger.Tags.create pool in
    Logs.info ~src (fun m -> m ~tags "Removing all default role permissions");
    Guard.RolePermissionsCleared |> Guard.handle_event pool
  ;;

  let restore_allowed_permissions pool =
    let tags = Database.Logger.Tags.create pool in
    Logs.info ~src (fun m -> m ~tags "Restoring demo allow-listed permissions");
    Guard.DefaultRestored AllowedPermissions.all |> Guard.handle_event pool
  ;;

  let create pool =
    let tags = Database.Logger.Tags.create pool in
    Logs.info ~src (fun m -> m ~tags "Initializing demo instance");
    let%lwt () = remove_all_permissions pool in
    let%lwt () = restore_allowed_permissions pool in
    Logs.info ~src (fun m -> m ~tags "Demo instance initialization complete");
    Lwt.return_unit
  ;;
end
