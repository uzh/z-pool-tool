let count_contacts_select =
  {sql|
    SELECT
      COUNT(*)
    FROM
      pool_contacts
      INNER JOIN user_users ON user_users.uuid = pool_contacts.user_uuid
    WHERE
      user_users.admin = 0
      AND user_users.confirmed = 1
      AND pool_contacts.email_verified IS NOT NULL
      AND pool_contacts.disabled = 0
      AND pool_contacts.paused = 0
      AND pool_contacts.import_pending = 0
  |sql}
;;

let active_contacts_request =
  let open Caqti_request.Infix in
  count_contacts_select |> Caqti_type.(unit ->! int)
;;

let active_contacts pool = Database.find pool active_contacts_request ()

let pending_contact_imports_request =
  let open Caqti_request.Infix in
  {sql|
    SELECT
      COUNT(*)
    FROM
      pool_contacts
      INNER JOIN user_users ON user_users.uuid = pool_contacts.user_uuid
    WHERE
      user_users.admin = 0
      AND pool_contacts.disabled = 0
      AND pool_contacts.import_pending = 1
  |sql}
  |> Caqti_type.(unit ->! int)
;;

let pending_contact_imports pool = Database.find pool pending_contact_imports_request ()

let login_count_request period =
  let open Caqti_request.Infix in
  Format.asprintf
    {sql|
      SELECT
        COUNT(*)
      FROM
        pool_contacts
      WHERE
        last_sign_in_at >= (NOW() - INTERVAL %s)
    |sql}
    (Entity.period_to_sql period)
  |> Caqti_type.(unit ->! int)
;;

let login_count pool period = Database.find pool (login_count_request period) ()

let sign_up_count_request period =
  let open Caqti_request.Infix in
  Format.asprintf
    {sql|
      SELECT
        COUNT(*)
      FROM
        pool_contacts
      INNER JOIN user_users
        ON pool_contacts.user_uuid = user_users.uuid
      WHERE
        user_users.created_at >= (NOW() - INTERVAL %s)
      AND
        pool_contacts.email_verified IS NOT NULL
      AND
        pool_contacts.terms_accepted_at IS NOT NULL
    |sql}
    (Entity.period_to_sql period)
  |> Caqti_type.(unit ->! int)
;;

let sign_up_count pool period = Database.find pool (sign_up_count_request period) ()

let assignments_created_request period =
  let open Caqti_request.Infix in
  Format.asprintf
    {sql|
      SELECT
        COUNT(*)
      FROM
        pool_assignments
      WHERE
        created_at >= (NOW() - INTERVAL %s)
        AND pool_assignments.canceled_at IS NULL
        AND pool_assignments.marked_as_deleted = 0
    |sql}
    (Entity.period_to_sql period)
  |> Caqti_type.(unit ->! int)
;;

let assignments_created pool period =
  Database.find pool (assignments_created_request period) ()
;;

let invitations_sent_request period =
  let open Caqti_request.Infix in
  Format.asprintf
    {sql|
      SELECT COUNT(*) FROM pool_invitations WHERE created_at >= (NOW() - INTERVAL %s);
    |sql}
    (Entity.period_to_sql period)
  |> Caqti_type.(unit ->! int)
;;

let invitations_sent pool period = Database.find pool (invitations_sent_request period) ()

let reminders_sent_request period =
  let open Caqti_request.Infix in
  Format.asprintf
    {sql|
      SELECT
        COUNT(*)
      FROM
        pool_assignments
        INNER JOIN pool_sessions ON pool_assignments.session_uuid = pool_sessions.uuid
      WHERE
        pool_sessions.email_reminder_sent_at >= (NOW() - INTERVAL %s)
        AND pool_assignments.marked_as_deleted = 0
        AND pool_assignments.canceled_at IS NULL
    |sql}
    (Entity.period_to_sql period)
  |> Caqti_type.(unit ->! int)
;;

let reminders_sent pool period = Database.find pool (reminders_sent_request period) ()

let terms_accepted_count_request period =
  let open Caqti_request.Infix in
  Format.asprintf
    {sql|
      %s
      AND pool_contacts.terms_accepted_at >= (NOW() - INTERVAL %s)
    |sql}
    count_contacts_select
    (Entity.period_to_sql period)
  |> Caqti_type.(unit ->! int)
;;

let terms_accepted_count pool period =
  Database.find pool (terms_accepted_count_request period) ()
;;

let total_emails_sent_request period =
  let open Caqti_request.Infix in
  Format.asprintf
    {sql|
      SELECT COUNT(*) FROM pool_queue_jobs_history
      WHERE handled_at >= (NOW() - INTERVAL %s)
    |sql}
    (Entity.period_to_sql period)
  |> Caqti_type.(unit ->! int)
;;

let total_emails_sent pool period =
  Database.find pool (total_emails_sent_request period) ()
;;
