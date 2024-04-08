let add_invitation_notifiaction_permission =
  Sihl.Database.Migration.create_step
    ~label:"add invitation notifiaction permission"
    {sql|
    INSERT INTO `guardian_role_permissions` (`role`, `permission`, `target_model`) VALUES
    ('`Recruiter', 'manage', '`InvitationNotification')
    ON DUPLICATE KEY UPDATE updated_at=updated_at;
  |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202403281032" |> add_step add_invitation_notifiaction_permission)
;;
