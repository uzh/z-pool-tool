module Common = Pool_common.Repo
open Entity

module RepoEntity = struct
  let make_caqti_type = Pool_common.Repo.make_caqti_type

  module Token = struct
    include Token

    let t = make_caqti_type Caqti_type.string create value
  end

  module ConfirmedAt = struct
    include ConfirmedAt

    let t = make_caqti_type Caqti_type.ptime create value
  end

  module NotifiedAt = struct
    include NotifiedAt

    let t = make_caqti_type Caqti_type.ptime create value
  end

  module ReminderCount = struct
    include ReminderCount

    let t = make_caqti_type Caqti_type.int create value
  end

  module LastRemindedAt = struct
    include LastRemindedAt

    let t = make_caqti_type Caqti_type.ptime create value
  end

  let t =
    let encode (m : t) =
      Ok
        ( m.user_uuid
        , ( m.token
          , ( m.confirmed_at
            , ( m.notified_at
              , ( m.reminder_count
                , (m.last_reminded_at, (m.created_at, m.updated_at)) ) ) ) ) )
    in
    let decode
      ( user_uuid
      , ( token
        , ( confirmed_at
          , ( notified_at
            , (reminder_count, (last_reminded_at, (created_at, updated_at))) )
          ) ) )
      =
      Ok
        { user_uuid
        ; token
        ; confirmed_at
        ; notified_at
        ; reminder_count
        ; last_reminded_at
        ; created_at
        ; updated_at
        }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2
           Common.Id.t
           (tup2
              Token.t
              (tup2
                 (option ConfirmedAt.t)
                 (tup2
                    (option NotifiedAt.t)
                    (tup2
                       ReminderCount.t
                       (tup2
                          (option LastRemindedAt.t)
                          (tup2 Common.CreatedAt.t Common.UpdatedAt.t))))))))
  ;;

  module Write = struct
    type t =
      { user_uuid : Pool_common.Id.t
      ; confirmed_at : ConfirmedAt.t option
      ; notified_at : NotifiedAt.t option
      ; reminder_count : ReminderCount.t
      ; last_reminded_at : LastRemindedAt.t option
      }

    let from_entity
      { Entity.user_uuid
      ; confirmed_at
      ; notified_at
      ; reminder_count
      ; last_reminded_at
      ; _
      }
      =
      { user_uuid; confirmed_at; notified_at; reminder_count; last_reminded_at }
    ;;

    let t =
      let encode (m : t) =
        Ok
          ( m.user_uuid
          , ( m.confirmed_at
            , (m.notified_at, (m.reminder_count, m.last_reminded_at)) ) )
      in
      let decode _ =
        failwith
          Pool_common.(
            Message.WriteOnlyModel |> Utils.error_to_string Language.En)
      in
      Caqti_type.(
        custom
          ~encode
          ~decode
          (tup2
             Common.Id.t
             (tup2
                (option ConfirmedAt.t)
                (tup2
                   (option NotifiedAt.t)
                   (tup2 ReminderCount.t (option LastRemindedAt.t))))))
    ;;
  end
end

let select_user_import_columns =
  {sql|
    LOWER(CONCAT(
      SUBSTR(HEX(pool_user_imports.user_uuid), 1, 8), '-',
      SUBSTR(HEX(pool_user_imports.user_uuid), 9, 4), '-',
      SUBSTR(HEX(pool_user_imports.user_uuid), 13, 4), '-',
      SUBSTR(HEX(pool_user_imports.user_uuid), 17, 4), '-',
      SUBSTR(HEX(pool_user_imports.user_uuid), 21)
    )),
    pool_user_imports.token,
    pool_user_imports.confirmed_at,
    pool_user_imports.notification_sent_at,
    pool_user_imports.reminder_count,
    pool_user_imports.last_reminder_sent_at,
    pool_user_imports.created_at,
    pool_user_imports.updated_at
  |sql}
;;

let select =
  Format.asprintf
    {sql|
    SELECT
      %s
    FROM pool_user_imports
    |sql}
    select_user_import_columns
;;

let find_pending_by_token_request =
  let open Caqti_request.Infix in
  Format.asprintf
    "%s\n%s"
    select
    {sql|
      WHERE token = $1
      AND confirmed_at IS NULL
    |sql}
  |> Caqti_type.string ->! RepoEntity.t
;;

let find_pending_by_token pool token =
  let open Utils.Lwt_result.Infix in
  Utils.Database.find_opt
    (Pool_database.Label.value pool)
    find_pending_by_token_request
    (Token.value token)
  ||> CCOption.to_result Pool_common.Message.(Invalid Field.Token)
;;

let find_pending_by_user_id_opt_request =
  let open Caqti_request.Infix in
  Format.asprintf
    "%s\n%s\n"
    select
    {sql|
      WHERE pool_user_imports.user_uuid = UNHEX(REPLACE($1, '-', ''))
      AND pool_user_imports.confirmed_at IS NULL
    |sql}
  |> Pool_common.Repo.Id.t ->! RepoEntity.t
;;

let find_pending_by_user_id_opt pool =
  Utils.Database.find_opt
    (Pool_database.Label.value pool)
    find_pending_by_user_id_opt_request
;;

let find_pending_by_email_opt_request =
  let open Caqti_request.Infix in
  Format.asprintf
    "%s\n%s\n"
    select
    {sql|
      INNER JOIN user_users
      ON user_users.uuid = pool_user_imports.user_uuid
      WHERE user_users.email = $1
      AND pool_user_imports.confirmed_at IS NULL
    |sql}
  |> Pool_user.Repo.EmailAddress.t ->! RepoEntity.t
;;

let find_pending_by_email_opt pool =
  Utils.Database.find_opt
    (Pool_database.Label.value pool)
    find_pending_by_email_opt_request
;;

let update_request =
  let open Caqti_request.Infix in
  {sql|
      UPDATE
        pool_user_imports
      SET
        confirmed_at = $2,
        notification_sent_at = $3,
        reminder_count = $4,
        last_reminder_sent_at = $5
      WHERE
        user_uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
  |> RepoEntity.Write.t ->. Caqti_type.unit
;;

let update pool t =
  Utils.Database.exec
    (Pool_database.Label.value pool)
    update_request
    (RepoEntity.Write.from_entity t)
;;

let find_admins_request ~where limit =
  let open Caqti_request.Infix in
  Admin.Repo.select_imported_admins_sql
    ~import_columns:select_user_import_columns
    ~where
    ~limit
  |> Caqti_type.(unit ->* tup2 Admin.Repo.Entity.t RepoEntity.t)
;;

let reminder_where_clause =
  {sql|
    pool_user_imports.notification_sent_at IS NOT NULL
    AND pool_user_imports.notification_sent_at < NOW() - INTERVAL 1 WEEK
    AND pool_user_imports.reminder_count < 2
    AND (
      pool_user_imports.last_reminder_sent_at IS NULL
      OR pool_user_imports.last_reminder_sent_at < NOW() - INTERVAL 1 WEEK)
  |sql}
;;

let find_admins_to_notify pool limit =
  Utils.Database.collect
    (Pool_database.Label.value pool)
    (find_admins_request
       ~where:
         {|pool_admins.import_pending = 1
          AND pool_user_imports.notification_sent_at IS NULL|}
       limit)
;;

let find_admins_to_remind pool limit =
  Utils.Database.collect
    (Pool_database.Label.value pool)
    (find_admins_request
       ~where:
         (Format.asprintf
            "pool_admins.import_pending = 1 AND %s"
            reminder_where_clause)
       limit)
;;

let find_contacts_request ~where limit =
  let open Caqti_request.Infix in
  Contact.Repo.Sql.select_imported_contacts_sql
    ~import_columns:select_user_import_columns
    ~where
    ~limit
  |> Caqti_type.(unit ->* tup2 Contact.Repo.Entity.t RepoEntity.t)
;;

let find_contacts_to_notify pool limit =
  Utils.Database.collect
    (Pool_database.Label.value pool)
    (find_contacts_request
       ~where:
         {| pool_contacts.import_pending = 1
          AND pool_contacts.disabled = 0
          AND pool_user_imports.notification_sent_at IS NULL|}
       limit)
;;

let find_contacts_to_remind pool limit =
  Utils.Database.collect
    (Pool_database.Label.value pool)
    (find_contacts_request
       ~where:
         (Format.asprintf
            {sql| pool_contacts.import_pending = 1
            AND pool_contacts.disabled = 0
            AND %s |sql}
            reminder_where_clause)
       limit)
;;
