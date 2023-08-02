open Entity

module RepoEntity = struct
  open CCFun.Infix

  let make_caqti_type = Pool_common.Repo.make_caqti_type

  module ActiveContacts = struct
    include ActiveContacts

    let t = make_caqti_type Caqti_type.int (of_int %> CCResult.return) value
  end

  module PendingContactImports = struct
    include PendingContactImports

    let t = make_caqti_type Caqti_type.int (of_int %> CCResult.return) value
  end

  module AssignmentsCreated = struct
    include AssignmentsCreated

    let t = make_caqti_type Caqti_type.int (of_int %> CCResult.return) value
  end

  module InvitationsSent = struct
    include InvitationsSent

    let t = make_caqti_type Caqti_type.int (of_int %> CCResult.return) value
  end

  module SignUpCount = struct
    include SignUpCount

    let t = make_caqti_type Caqti_type.int (of_int %> CCResult.return) value
  end
end

let active_contacts_request =
  let open Caqti_request.Infix in
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
  |sql}
  |> Caqti_type.(unit ->! RepoEntity.ActiveContacts.t)
;;

let active_contacts pool =
  Utils.Database.find
    (Pool_database.Label.value pool)
    active_contacts_request
    ()
;;

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
      AND pool_contacts.import_pending = 1
  |sql}
  |> Caqti_type.(unit ->! RepoEntity.PendingContactImports.t)
;;

let pending_contact_imports pool =
  Utils.Database.find
    (Pool_database.Label.value pool)
    pending_contact_imports_request
    ()
;;

let assignments_created_request =
  let open Caqti_request.Infix in
  {sql|
    SELECT
      COUNT(*)
    FROM
      pool_assignments
    WHERE
      created_at >= (NOW() - INTERVAL $1)
      AND pool_assignments.canceled_at IS NULL
      AND pool_assignments.marked_as_deleted = 0
  |sql}
  |> Caqti_type.(string ->! RepoEntity.AssignmentsCreated.t)
;;

let assignments_created pool period =
  Utils.Database.find
    (Pool_database.Label.value pool)
    assignments_created_request
    (Entity.period_to_sql period)
;;

let invitations_sent_request =
  let open Caqti_request.Infix in
  {sql|
    SELECT COUNT(*) FROM pool_invitations WHERE created_at >= (NOW() - INTERVAL $1);
    |sql}
  |> Caqti_type.(string ->! RepoEntity.InvitationsSent.t)
;;

let invitations_sent pool period =
  Utils.Database.find
    (Pool_database.Label.value pool)
    invitations_sent_request
    (Entity.period_to_sql period)
;;

let sign_up_count_request =
  let open Caqti_request.Infix in
  {sql|
    SELECT
      COUNT(*)
    FROM
      pool_contacts
    WHERE
      last_sign_in_at >= (NOW() - INTERVAL $1)
  |sql}
  |> Caqti_type.(string ->! RepoEntity.SignUpCount.t)
;;

let sign_up_count pool period =
  Utils.Database.find
    (Pool_database.Label.value pool)
    sign_up_count_request
    (Entity.period_to_sql period)
;;
