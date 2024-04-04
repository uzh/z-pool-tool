let add_email_verified_column_to_admin =
  Sihl.Database.Migration.create_step
    ~label:"add email verified column to admin"
    {sql|
      ALTER TABLE pool_admins
        ADD COLUMN email_verified timestamp NULL DEFAULT NULL AFTER user_uuid
    |sql}
;;

let add_email_verified_timestamp_to_existing_admins =
  Sihl.Database.Migration.create_step
    ~label:"add email_verified timestamp to existing admins"
    {sql|
      UPDATE
        pool_admins
      SET
        email_verified = NOW()
      WHERE (
        SELECT
          confirmed
        FROM
          user_users
        WHERE
          user_users.uuid = pool_admins.user_uuid) = 1
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202312200952"
    |> add_step add_email_verified_column_to_admin
    |> add_step add_email_verified_timestamp_to_existing_admins)
;;
