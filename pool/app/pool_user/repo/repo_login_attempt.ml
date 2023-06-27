open Login_attempt_entity

module RepoEntity = struct
  module Counter = struct
    include Counter

    let t =
      Pool_common.Repo.make_caqti_type
        Caqti_type.int
        CCFun.(create %> CCResult.return)
        value
    ;;
  end

  module BlockedUntil = struct
    include BlockedUntil

    let t = Caqti_type.ptime
  end

  let t =
    let encode m = Ok (m.id, (m.email, (m.counter, m.blocked_until))) in
    let decode (id, (email, (counter, blocked_until))) =
      let open CCResult in
      Ok { id; email; counter; blocked_until }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2
           Pool_common.Repo.Id.t
           (tup2
              Repo_model.EmailAddress.t
              (tup2 Counter.t (option BlockedUntil.t)))))
  ;;
end

let select_sql =
  Format.asprintf
    {sql|
      SELECT
        LOWER(CONCAT(
          SUBSTR(HEX(uuid), 1, 8), '-',
          SUBSTR(HEX(uuid), 9, 4), '-',
          SUBSTR(HEX(uuid), 13, 4), '-',
          SUBSTR(HEX(uuid), 17, 4), '-',
          SUBSTR(HEX(uuid), 21)
        )),
        email,
        counter,
        blocked_until
      FROM
        pool_failed_login_attempts
      WHERE
        %s
  |sql}
;;

let find_opt_request =
  let open Caqti_request.Infix in
  {sql|
      email = ?
  |sql}
  |> select_sql
  |> Caqti_type.string ->! RepoEntity.t
;;

let find_opt pool email =
  Utils.Database.find_opt
    (Pool_database.Label.value pool)
    find_opt_request
    (Entity.EmailAddress.value email)
;;

let insert_request =
  let open Caqti_request.Infix in
  {sql|
    INSERT INTO pool_failed_login_attempts (
      uuid,
      email,
      counter,
      blocked_until
    ) VALUES (
      UNHEX(REPLACE($1, '-', '')),
      $2,
      $3,
      $4
    )
    ON DUPLICATE KEY UPDATE
      counter = $3,
      blocked_until = $4
  |sql}
  |> RepoEntity.t ->. Caqti_type.unit
;;

let insert pool =
  Utils.Database.exec (Pool_database.Label.value pool) insert_request
;;

let delete_request =
  let open Caqti_request.Infix in
  {sql|
    DELETE FROM pool_failed_login_attempts
    WHERE email = $1
  |sql}
  |> Caqti_type.(string ->. unit)
;;

let delete pool t =
  Utils.Database.exec
    (Pool_database.Label.value pool)
    delete_request
    (t.email |> Entity.EmailAddress.value)
;;
