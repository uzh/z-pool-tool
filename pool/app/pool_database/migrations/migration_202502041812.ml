let allow_recruiters_to_manage_duplicates =
  Database.Migration.Step.create
    ~label:"allow recruiters to manage duplicates"
    {sql|
    INSERT INTO `guardian_role_permissions` (`role`, `permission`, `target_model`) VALUES
    ('`Recruiter', 'manage', '`DuplicateContact')
    ON DUPLICATE KEY UPDATE updated_at=updated_at;
  |sql}
;;

let migration () =
  Database.Migration.(
    empty "202502041812" |> add_step allow_recruiters_to_manage_duplicates)
;;
