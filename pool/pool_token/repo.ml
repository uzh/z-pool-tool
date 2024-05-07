open Ppx_yojson_conv_lib.Yojson_conv

module Model = struct
  module Data = struct
    type t = (string * string) list [@@deriving eq, show, yojson]

    let to_string data = data |> yojson_of_t |> Yojson.Safe.to_string
    let of_string str = str |> Yojson.Safe.from_string |> t_of_yojson
  end

  module Status = struct
    type t =
      | Active
      | Inactive
    [@@deriving eq]

    let to_string = function
      | Active -> "active"
      | Inactive -> "inactive"
    ;;

    let of_string str =
      match str with
      | "active" -> Ok Active
      | "inactive" -> Ok Inactive
      | _ -> Error (Printf.sprintf "Invalid token status %s provided" str)
    ;;
  end

  type t =
    { id : string
    ; value : string
    ; data : Data.t
    ; status : Status.t
    ; expires_at : Ptime.t
    ; created_at : Ptime.t
    }

  let t =
    let ( let* ) = CCResult.( >>= ) in
    let encode m =
      let status = Status.to_string m.status in
      let data = Data.to_string m.data in
      Ok (m.id, (m.value, (data, (status, (m.expires_at, m.created_at)))))
    in
    let decode (id, (value, (data, (status, (expires_at, created_at))))) =
      let* status = Status.of_string status in
      let* data =
        try Data.of_string data |> CCResult.return with
        | _ -> Error "Failed to decode Token"
      in
      Ok { id; value; data; status; expires_at; created_at }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (t2 string (t2 string (t2 string (t2 string (t2 ptime ptime))))))
  ;;
end

let lifecycles = [ Pool_database.lifecycle ]

let label =
  let open CCFun.Infix in
  CCOption.(map Database.of_ctx_exn %> get_exn_or "Database: Invalid context")
;;

module Sql = struct
  let find_request =
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
          token_value,
          token_data,
          status,
          expires_at,
          created_at
        FROM token_tokens
        WHERE token_tokens.token_value = ?
      |sql}
    |> Caqti_type.string ->! Model.t
  ;;

  let find label = Database.find label find_request

  let find_request_opt =
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
          token_value,
          token_data,
          status,
          expires_at,
          created_at
        FROM token_tokens
        WHERE token_tokens.token_value = ?
      |sql}
    |> Caqti_type.string ->? Model.t
  ;;

  let find_opt label = Database.find_opt label find_request_opt

  let find_by_id_request =
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
          token_value,
          token_data,
          status,
          expires_at,
          created_at
        FROM token_tokens
        WHERE token_tokens.uuid = UNHEX(REPLACE(?, '-', ''))
      |sql}
    |> Caqti_type.string ->! Model.t
  ;;

  let find_by_id label = Database.find label find_by_id_request

  let insert_request =
    let open Caqti_request.Infix in
    {sql|
        INSERT INTO token_tokens (
          uuid,
          token_value,
          token_data,
          status,
          expires_at,
          created_at
        ) VALUES (
          UNHEX(REPLACE($1, '-', '')),
          $2,
          $3,
          $4,
          $5,
          $6
        )
      |sql}
    |> Model.t ->. Caqti_type.unit
  ;;

  let insert label = Database.exec label insert_request

  let update_request =
    let open Caqti_request.Infix in
    {sql|
        UPDATE token_tokens
        SET
          token_data = $3,
          status = $4,
          expires_at = $5,
          created_at = $6
        WHERE token_tokens.token_value = $2
      |sql}
    |> Model.t ->. Caqti_type.unit
  ;;

  let update label = Database.exec label update_request

  let clean_request =
    let open Caqti_request.Infix in
    "TRUNCATE token_tokens" |> Caqti_type.(unit ->. unit)
  ;;

  let clean label () = Database.exec label clean_request ()
end

module Migration = struct
  module Migration = Database.Migration

  let fix_collation =
    Migration.Step.create
      ~label:"fix collation"
      "SET collation_server = 'utf8mb4_unicode_ci'"
  ;;

  let create_tokens_table =
    Migration.Step.create
      ~label:"create tokens table"
      {sql|
          CREATE TABLE IF NOT EXISTS token_tokens (
            id BIGINT UNSIGNED AUTO_INCREMENT,
            uuid BINARY(16) NOT NULL,
            token_value VARCHAR(128) NOT NULL,
            token_data VARCHAR(1024),
            token_kind VARCHAR(128) NOT NULL,
            status VARCHAR(128) NOT NULL,
            expires_at TIMESTAMP NOT NULL,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
          PRIMARY KEY (id),
          CONSTRAINT unqiue_uuid UNIQUE KEY (uuid),
          CONSTRAINT unique_value UNIQUE KEY (token_value)
          ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
        |sql}
  ;;

  let remove_token_kind_column =
    Migration.Step.create
      ~label:"remove token kind column"
      "ALTER TABLE token_tokens DROP COLUMN token_kind"
  ;;

  let migration () =
    Migration.(
      empty "tokens"
      |> add_step fix_collation
      |> add_step create_tokens_table
      |> add_step remove_token_kind_column)
  ;;
end

let register_migration () =
  Database.Migration.register_migration (Migration.migration ())
;;

let register_cleaner () =
  Sihl.Cleaner.register_cleaner (fun ?(ctx = []) ->
    Sql.clean (Database.of_ctx_exn ctx))
;;

let find = Sql.find
let find_opt = Sql.find_opt
let find_by_id = Sql.find_by_id
let insert = Sql.insert
let update = Sql.update
