let add_default_guardian_role_permission =
  Database.Migration.Step.create
    ~label:"add default guardian role permissions"
    {sql|
    INSERT INTO `guardian_role_permissions` (`role`, `permission`, `target_model`) VALUES
    ('`Operator', 'manage', '`Admin'),
    ('`Operator', 'manage', '`Assignment'),
    ('`Operator', 'manage', '`Contact'),
    ('`Operator', 'manage', '`ContactInfo'),
    ('`Operator', 'manage', '`ContactName'),
    ('`Operator', 'manage', '`CustomField'),
    ('`Operator', 'manage', '`CustomFieldGroup'),
    ('`Operator', 'manage', '`Experiment'),
    ('`Operator', 'manage', '`Filter'),
    ('`Operator', 'manage', '`I18n'),
    ('`Operator', 'manage', '`Invitation'),
    ('`Operator', 'manage', '`Location'),
    ('`Operator', 'manage', '`LocationFile'),
    ('`Operator', 'manage', '`Message'),
    ('`Operator', 'manage', '`MessageTemplate'),
    ('`Operator', 'manage', '`OrganisationalUnit'),
    ('`Operator', 'manage', '`Mailing'),
    ('`Operator', 'manage', '`Permission'),
    ('`Operator', 'manage', '`Queue'),
    ('`Operator', 'manage', '`Role'),
    ("`Operator", "manage", "`RoleAdmin"),
    ("`Operator", "manage", "`RoleAssistant"),
    ("`Operator", "manage", "`RoleExperimenter"),
    ("`Operator", "manage", "`RoleLocationManager"),
    ("`Operator", "manage", "`RoleRecruiter"),
    ("`Operator", "manage", "`RoleOperator"),
    ('`Operator', 'manage', '`Schedule'),
    ('`Operator', 'manage', '`Session'),
    ('`Operator', 'manage', '`SessionClose'),
    ('`Operator', 'manage', '`SystemSetting'),
    ('`Operator', 'manage', '`Smtp'),
    ('`Operator', 'manage', '`Statistics'),
    ('`Operator', 'manage', '`System'),
    ('`Operator', 'manage', '`Tag'),
    ('`Operator', 'manage', '`Tenant'),
    ('`Operator', 'manage', '`WaitingList'),
    ('`Recruiter', 'read', '`Admin'),
    ('`Recruiter', 'manage', '`Assignment'),
    ('`Recruiter', 'manage', '`Contact'),
    ('`Recruiter', 'manage', '`CustomField'),
    ('`Recruiter', 'manage', '`CustomFieldGroup'),
    ('`Recruiter', 'manage', '`Experiment'),
    ('`Recruiter', 'manage', '`Filter'),
    ('`Recruiter', 'manage', '`I18n'),
    ('`Recruiter', 'manage', '`Invitation'),
    ('`Recruiter', 'manage', '`Location'),
    ('`Recruiter', 'manage', '`LocationFile'),
    ('`Recruiter', 'manage', '`Mailing'),
    ('`Recruiter', 'manage', '`MessageTemplate'),
    ('`Recruiter', 'read', '`Permission'),
    ('`Recruiter', 'read', '`OrganisationalUnit'),
    ('`Recruiter', 'read', '`Queue'),
    ('`Recruiter', 'manage', '`Role'),
    ('`Recruiter', 'read', '`Schedule'),
    ('`Recruiter', 'manage', '`Session'),
    ('`Recruiter', 'read', '`SystemSetting'),
    ('`Recruiter', 'read', '`Smtp'),
    ('`Recruiter', 'read', '`Statistics'),
    ('`Recruiter', 'read', '`System'),
    ('`Recruiter', 'manage', '`Tag'),
    ('`Recruiter', 'read', '`Tenant'),
    ('`Recruiter', 'manage', '`WaitingList'),
    ('`Assistant', 'read', '`Contact'),
    ('`Assistant', 'update', '`Contact'),
    ('`Assistant', 'create', '`Message'),
    ('`Assistant', 'read', '`Session'),
    ('`Assistant', 'read', '`SessionClose'),
    ('`Assistant', 'update', '`SessionClose'),
    ('`Assistant', 'read', '`Assignment'),
    ('`Assistant', 'update', '`Assignment'),
    ('`Assistant', 'read', '`Experiment'),
    ('`Experimenter', 'read', '`ContactName'),
    ('`Experimenter', 'read', '`Session'),
    ('`Experimenter', 'read', '`SessionClose'),
    ('`Experimenter', 'update', '`SessionClose'),
    ('`Experimenter', 'read', '`Assignment'),
    ('`Experimenter', 'update', '`Assignment'),
    ('`Experimenter', 'read', '`Experiment'),
    ('`LocationManager', 'manage', '`Location'),
    ('`LocationManager', 'manage', '`LocationFile'),
    ('`LocationManager', 'read', '`ContactName'),
    ('`LocationManager', 'read', '`ContactInfo'),
    ('`LocationManager', 'read', '`Session')
    ON DUPLICATE KEY UPDATE updated_at=updated_at;
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202309211305" |> add_step add_default_guardian_role_permission)
;;

let add_default_guardian_role_permission_root =
  Database.Migration.Step.create
    ~label:"add default guardian role permissions"
    {sql|
    INSERT INTO `guardian_role_permissions` (`role`, `permission`, `target_model`) VALUES
    ('`Operator', 'manage', '`Admin'),
    ('`Operator', 'manage', '`Assignment'),
    ('`Operator', 'manage', '`Contact'),
    ('`Operator', 'manage', '`ContactInfo'),
    ('`Operator', 'manage', '`ContactName'),
    ('`Operator', 'manage', '`CustomField'),
    ('`Operator', 'manage', '`CustomFieldGroup'),
    ('`Operator', 'manage', '`Experiment'),
    ('`Operator', 'manage', '`Filter'),
    ('`Operator', 'manage', '`I18n'),
    ('`Operator', 'manage', '`Invitation'),
    ('`Operator', 'manage', '`Location'),
    ('`Operator', 'manage', '`LocationFile'),
    ('`Operator', 'manage', '`Message'),
    ('`Operator', 'manage', '`MessageTemplate'),
    ('`Operator', 'manage', '`OrganisationalUnit'),
    ('`Operator', 'manage', '`Mailing'),
    ('`Operator', 'manage', '`Permission'),
    ('`Operator', 'manage', '`Queue'),
    ('`Operator', 'manage', '`Role'),
    ('`Operator', 'manage', '`Schedule'),
    ('`Operator', 'manage', '`Session'),
    ('`Operator', 'manage', '`SessionClose'),
    ('`Operator', 'manage', '`SystemSetting'),
    ('`Operator', 'manage', '`Smtp'),
    ('`Operator', 'manage', '`Statistics'),
    ('`Operator', 'manage', '`System'),
    ('`Operator', 'manage', '`Tag'),
    ('`Operator', 'manage', '`Tenant'),
    ('`Operator', 'manage', '`WaitingList')
    ON DUPLICATE KEY UPDATE updated_at=updated_at;
    |sql}
;;

let migration_root () =
  Database.Migration.(
    empty "202309211305" |> add_step add_default_guardian_role_permission_root)
;;
