let add_direct_message_permission =
  Database.Migration.create_step
    ~label:"add direct message permission"
    {sql|
    INSERT INTO `guardian_role_permissions` (`role`, `permission`, `target_model`) VALUES
    ('`Recruiter', 'manage', '`ContactDirectMessage'),
    ('`Operator', 'manage', '`ContactDirectMessage')
    ON DUPLICATE KEY UPDATE updated_at=updated_at;
  |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202403181624" |> add_step add_direct_message_permission)
;;
