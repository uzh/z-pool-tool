module Cleaner = Sihl.Cleaner
module Model = Sihl.Contract.User

module type Sig = sig
  val register_migration : unit -> unit
  val register_cleaner : unit -> unit
  val lifecycles : Sihl.Container.lifecycle list

  val search
    :  ?ctx:(string * string) list
    -> [ `Desc | `Asc ]
    -> string option
    -> limit:int
    -> offset:int
    -> (Model.t list * int) Lwt.t

  val get : ?ctx:(string * string) list -> string -> Model.t option Lwt.t

  val get_by_email
    :  ?ctx:(string * string) list
    -> string
    -> Model.t option Lwt.t

  val insert : ?ctx:(string * string) list -> Model.t -> unit Lwt.t
  val update : ?ctx:(string * string) list -> Model.t -> unit Lwt.t
end

let status =
  let encode m = m |> Model.status_to_string |> Result.ok in
  let decode = Model.status_of_string in
  Caqti_type.(custom ~encode ~decode string)
;;

let user =
  let open Sihl.Contract.User in
  let encode m =
    Ok
      ( m.id
      , ( m.email
        , ( m.username
          , ( m.name
            , ( m.given_name
              , ( m.password
                , ( m.status
                  , (m.admin, (m.confirmed, (m.created_at, m.updated_at))) ) )
              ) ) ) ) )
  in
  let decode
    ( id
    , ( email
      , ( username
        , ( name
          , ( given_name
            , ( password
              , (status, (admin, (confirmed, (created_at, updated_at)))) ) ) )
        ) ) )
    =
    Ok
      { id
      ; email
      ; username
      ; name
      ; given_name
      ; password
      ; status
      ; admin
      ; confirmed
      ; created_at
      ; updated_at
      }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (t2
         string
         (t2
            string
            (t2
               (option string)
               (t2
                  (option string)
                  (t2
                     (option string)
                     (t2
                        string
                        (t2 status (t2 bool (t2 bool (t2 ptime ptime)))))))))))
;;

let lifecycles = [ Pool_database.lifecycle ]

module Migration = struct
  open Database.Migration

  let fix_collation =
    Step.create
      ~label:"fix collation"
      {sql|
        SET collation_server = 'utf8mb4_unicode_ci'
      |sql}
  ;;

  let create_users_table =
    Step.create
      ~label:"create users table"
      {sql|
        CREATE TABLE IF NOT EXISTS user_users (
          id BIGINT UNSIGNED AUTO_INCREMENT,
          uuid BINARY(16) NOT NULL,
          email VARCHAR(128) NOT NULL,
          password VARCHAR(128) NOT NULL,
          username VARCHAR(128),
          status VARCHAR(128) NOT NULL,
          admin BOOLEAN NOT NULL DEFAULT false,
          confirmed BOOLEAN NOT NULL DEFAULT false,
          created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
          PRIMARY KEY (id),
          CONSTRAINT unique_uuid UNIQUE KEY (uuid),
          CONSTRAINT unique_email UNIQUE KEY (email)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
      |sql}
  ;;

  let add_updated_at_column =
    Step.create
      ~label:"add updated_at column"
      {sql|
        ALTER TABLE user_users
          ADD COLUMN updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
      |sql}
  ;;

  let add_name_columns =
    Step.create
      ~label:"add name columns"
      {sql|
        ALTER TABLE user_users
          ADD COLUMN name VARCHAR(128) NULL,
          ADD COLUMN given_name VARCHAR(128) NULL
      |sql}
  ;;

  let migration () =
    empty "user"
    |> add_step fix_collation
    |> add_step create_users_table
    |> add_step add_updated_at_column
    |> add_step add_name_columns
  ;;
end

let filter_fragment =
  {sql|
    WHERE user_users.email LIKE $1
      OR user_users.username LIKE $1
      OR user_users.name LIKE $1
      OR user_users.given_name LIKE $1
      OR user_users.status LIKE $1
  |sql}
;;

let search_query =
  {sql|
    SELECT
      COUNT(*) OVER() as total,
      LOWER(CONCAT(
        SUBSTR(HEX(uuid), 1, 8), '-',
        SUBSTR(HEX(uuid), 9, 4), '-',
        SUBSTR(HEX(uuid), 13, 4), '-',
        SUBSTR(HEX(uuid), 17, 4), '-',
        SUBSTR(HEX(uuid), 21)
        )),
      email,
      username,
      name,
      given_name,
      password,
      status,
      admin,
      confirmed,
      created_at,
      updated_at
    FROM user_users
  |sql}
;;

let request =
  Sihl.Database.prepare_search_request
    ~search_query
    ~filter_fragment
    ~sort_by_field:"id"
    user
;;

(* TODO: Remove Sihl Database dependency *)
let search ?ctx sort filter ~limit ~offset =
  Sihl.Database.run_search_request ?ctx request sort filter ~limit ~offset
;;

let get_request =
  let open Caqti_request.Infix in
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
        username,
        name,
        given_name,
        password,
        status,
        admin,
        confirmed,
        created_at,
        updated_at
      FROM user_users
      WHERE user_users.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
  |> Caqti_type.string ->? user
;;

let get label id = Database.find_opt label get_request id

let get_by_email_request =
  let open Caqti_request.Infix in
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
        username,
        name,
        given_name,
        password,
        status,
        admin,
        confirmed,
        created_at,
        updated_at
      FROM user_users
      WHERE user_users.email LIKE ?
    |sql}
  |> Caqti_type.string ->? user
;;

let get_by_email label email =
  Database.find_opt label get_by_email_request email
;;

let insert_request =
  let open Caqti_request.Infix in
  {sql|
      INSERT INTO user_users (
        uuid,
        email,
        username,
        name,
        given_name,
        password,
        status,
        admin,
        confirmed,
        created_at,
        updated_at
      ) VALUES (
        UNHEX(REPLACE($1, '-', '')),
        LOWER($2),
        $3,
        $4,
        $5,
        $6,
        $7,
        $8,
        $9,
        $10,
        $11
      )
    |sql}
  |> user ->. Caqti_type.unit
;;

let insert label user = Database.exec label insert_request user

let update_request =
  let open Caqti_request.Infix in
  {sql|
      UPDATE user_users
      SET
        email = LOWER($2),
        username = $3,
        name = $4,
        given_name = $5,
        password = $6,
        status = $7,
        admin = $8,
        confirmed = $9,
        created_at = $10,
        updated_at = $11
      WHERE user_users.uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
  |> user ->. Caqti_type.unit
;;

let update label user = Database.exec label update_request user

let clean_request =
  let open Caqti_request.Infix in
  "TRUNCATE user_users" |> Caqti_type.(unit ->. unit)
;;

let clean label () = Database.exec label clean_request ()

let register_migration () =
  Database.Migration.register_migration (Migration.migration ())
;;

let register_cleaner () =
  Cleaner.register_cleaner (fun ?ctx () ->
    clean
      CCOption.(
        map Database.of_ctx_exn ctx
        |> get_exn_or Pool_message.(Error.(NotFound Field.Context |> show)))
      ())
;;
