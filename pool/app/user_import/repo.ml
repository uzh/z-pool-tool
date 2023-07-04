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

  module RemindedAt = struct
    include RemindedAt

    let t = make_caqti_type Caqti_type.ptime create value
  end

  let t =
    let encode (m : t) =
      Ok
        ( m.user_uuid
        , ( m.token
          , ( m.confirmed_at
            , (m.notified_at, (m.reminded_at, (m.created_at, m.updated_at))) )
          ) )
    in
    let decode
      ( user_uuid
      , ( token
        , (confirmed_at, (notified_at, (reminded_at, (created_at, updated_at))))
        ) )
      =
      Ok
        { user_uuid
        ; token
        ; confirmed_at
        ; notified_at
        ; reminded_at
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
                       (option RemindedAt.t)
                       (tup2 Common.CreatedAt.t Common.UpdatedAt.t)))))))
  ;;

  module Write = struct
    type t =
      { user_uuid : Pool_common.Id.t
      ; confirmed_at : ConfirmedAt.t option
      ; notified_at : NotifiedAt.t option
      ; reminded_at : RemindedAt.t option
      }

    let from_entity
      { Entity.user_uuid; confirmed_at; notified_at; reminded_at; _ }
      =
      { user_uuid; confirmed_at; notified_at; reminded_at }
    ;;

    let t =
      let encode (m : t) =
        Ok (m.user_uuid, (m.confirmed_at, (m.notified_at, m.reminded_at)))
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
                (tup2 (option NotifiedAt.t) (option RemindedAt.t)))))
    ;;
  end
end

let select =
  {sql|
    SELECT
      LOWER(CONCAT(
        SUBSTR(HEX(user_uuid), 1, 8), '-',
        SUBSTR(HEX(user_uuid), 9, 4), '-',
        SUBSTR(HEX(user_uuid), 13, 4), '-',
        SUBSTR(HEX(user_uuid), 17, 4), '-',
        SUBSTR(HEX(user_uuid), 21)
      )),
      token,
      confirmed_at,
      notification_sent_at,
      reminder_sent_at,
      created_at,
      updated_at
    FROM pool_user_imports
    |sql}
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

let find_pending_by_user_request =
  let open Caqti_request.Infix in
  Format.asprintf
    "%s\n%s"
    select
    {sql|
      WHERE user_uuid = UNHEX(REPLACE($1, '-', ''))
      AND confirmed_at IS NULL
    |sql}
  |> Caqti_type.string ->! RepoEntity.t
;;

let find_pending_by_user_opt pool user =
  Utils.Database.find_opt
    (Pool_database.Label.value pool)
    find_pending_by_user_request
    user.Sihl_user.id
;;

let update_request =
  let open Caqti_request.Infix in
  {sql|
      UPDATE
        pool_user_imports
      SET
        confirmed_at = $2,
        notification_sent_at = $3,
        reminder_sent_at = $4
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
