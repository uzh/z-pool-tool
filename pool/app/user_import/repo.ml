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
        (t2
           Pool_user.Repo.Id.t
           (t2
              Token.t
              (t2
                 (option ConfirmedAt.t)
                 (t2
                    (option NotifiedAt.t)
                    (t2
                       ReminderCount.t
                       (t2
                          (option LastRemindedAt.t)
                          (t2 Common.CreatedAt.t Common.UpdatedAt.t))))))))
  ;;

  let reminder_settings_caqti =
    let open Settings.UserImportReminder in
    let encode (first_reminder, second_reminder) =
      Ok
        ( FirstReminderAfter.value first_reminder
        , SecondReminderAfter.value second_reminder )
    in
    let decode _ =
      Pool_message.Error.WriteOnlyModel |> Pool_common.Utils.failwith
    in
    Caqti_type.(custom ~encode ~decode (t2 ptime_span ptime_span))
  ;;

  module Write = struct
    type t =
      { user_uuid : Pool_user.Id.t
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
        Pool_message.Error.WriteOnlyModel |> Pool_common.Utils.failwith
      in
      Caqti_type.(
        custom
          ~encode
          ~decode
          (t2
             Pool_user.Repo.Id.t
             (t2
                (option ConfirmedAt.t)
                (t2
                   (option NotifiedAt.t)
                   (t2 ReminderCount.t (option LastRemindedAt.t))))))
    ;;
  end
end

let sql_select_columns =
  [ Pool_common.Id.sql_select_fragment ~field:"pool_user_imports.user_uuid"
  ; "pool_user_imports.token"
  ; "pool_user_imports.confirmed_at"
  ; "pool_user_imports.notification_sent_at"
  ; "pool_user_imports.reminder_count"
  ; "pool_user_imports.last_reminder_sent_at"
  ; "pool_user_imports.created_at"
  ; "pool_user_imports.updated_at"
  ]
;;

let joins =
  {sql|
    INNER JOIN pool_user_imports
      ON user_users.uuid = pool_user_imports.user_uuid
  |sql}
;;

let select =
  Format.asprintf
    {sql|
    SELECT
      %s
    FROM pool_user_imports
    |sql}
    (sql_select_columns |> CCString.concat ", ")
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
  Database.find_opt pool find_pending_by_token_request (Token.value token)
  ||> CCOption.to_result Pool_message.(Error.Invalid Field.Token)
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
  |> Pool_user.Repo.Id.t ->! RepoEntity.t
;;

let find_pending_by_user_id_opt pool =
  Database.find_opt pool find_pending_by_user_id_opt_request
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
  Database.find_opt pool find_pending_by_email_opt_request
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
  Database.exec pool update_request (RepoEntity.Write.from_entity t)
;;

let find_admins_request ~where limit =
  Format.asprintf
    {sql|
      SELECT
        %s
      FROM pool_admins
        %s
      WHERE
        %s
      ORDER BY
        pool_admins.created_at ASC
      LIMIT %i
    |sql}
    (Admin.Repo.sql_select_columns @ sql_select_columns |> CCString.concat ", ")
    ([ Admin.Repo.joins; joins ] |> CCString.concat "\n")
    where
    limit
;;

let reminder_where_clause =
  {sql|
    pool_user_imports.notification_sent_at IS NOT NULL
    AND pool_user_imports.notification_sent_at < NOW() - INTERVAL $1 SECOND
    AND pool_user_imports.reminder_count < 2
    AND (
      pool_user_imports.last_reminder_sent_at IS NULL
      OR pool_user_imports.last_reminder_sent_at < NOW() - INTERVAL $2 SECOND)
  |sql}
;;

let find_admins_to_notify pool limit =
  let open Caqti_request.Infix in
  Database.collect
    pool
    (find_admins_request
       ~where:
         {|pool_admins.import_pending = 1
          AND pool_user_imports.notification_sent_at IS NULL|}
       limit
     |> Caqti_type.(unit ->* t2 Admin.Repo.Entity.t RepoEntity.t))
;;

let find_admins_to_remind reminder_settings pool limit () =
  let open Caqti_request.Infix in
  let request =
    find_admins_request
      ~where:
        (Format.asprintf
           "pool_admins.import_pending = 1 AND %s"
           reminder_where_clause)
      limit
    |> Caqti_type.(
         RepoEntity.reminder_settings_caqti
         ->* t2 Admin.Repo.Entity.t RepoEntity.t)
  in
  Database.collect pool request reminder_settings
;;

let find_contacts_request ~where limit =
  Format.asprintf
    {sql|
      SELECT
        %s
      FROM pool_contacts
        %s
      WHERE
        %s
      ORDER BY
        pool_contacts.created_at ASC
      LIMIT %i
    |sql}
    (Contact.Repo.sql_select_columns @ sql_select_columns
     |> CCString.concat ", ")
    ([ Contact.Repo.joins; joins ] |> CCString.concat "\n")
    where
    limit
;;

let find_contacts_to_notify pool limit =
  let open Caqti_request.Infix in
  Database.collect
    pool
    (find_contacts_request
       ~where:
         {| pool_contacts.import_pending = 1
          AND pool_contacts.disabled = 0
          AND pool_user_imports.notification_sent_at IS NULL|}
       limit
     |> Caqti_type.(unit ->* t2 Contact.Repo.Entity.t RepoEntity.t))
;;

let find_contacts_to_remind reminder_settings pool limit () =
  let open Caqti_request.Infix in
  let request =
    find_contacts_request
      ~where:
        (Format.asprintf
           {sql| pool_contacts.import_pending = 1
         AND pool_contacts.disabled = 0
         AND %s |sql}
           reminder_where_clause)
      limit
    |> Caqti_type.(
         RepoEntity.reminder_settings_caqti
         ->* t2 Contact.Repo.Entity.t RepoEntity.t)
  in
  Database.collect pool request reminder_settings
;;

let insert_request =
  let open Caqti_request.Infix in
  {sql|
    INSERT INTO pool_user_imports (
      user_uuid,
      token
    ) VALUES (
      UNHEX(REPLACE($1, '-', '')),
      $2
    )
  |sql}
  |> Caqti_type.(t2 Pool_user.Repo.Id.t string ->. unit)
;;

let insert pool t =
  Database.exec pool insert_request (t.user_uuid, t.token |> Token.value)
;;
