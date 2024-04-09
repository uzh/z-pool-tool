module Migration = struct
  type t =
    { namespace : string
    ; version : int
    ; dirty : bool
    }
  [@@deriving fields, eq, show]

  let create ~namespace = { namespace; version = 0; dirty = true }
  let mark_dirty state = { state with dirty = true }
  let mark_clean state = { state with dirty = false }
  let increment state = { state with version = state.version + 1 }

  let steps_to_apply (namespace, steps) { version; _ } =
    namespace, CCList.drop version steps
  ;;

  let of_tuple (namespace, version, dirty) = { namespace; version; dirty }
  let to_tuple state = state.namespace, state.version, state.dirty
  let dirty state = state.dirty
end

module type Sig = sig
  module Migration = Migration

  val create_table_if_not_exists : Entity.Label.t -> string -> unit Lwt.t

  val get
    :  Entity.Label.t
    -> string
    -> namespace:string
    -> Migration.t option Lwt.t

  val get_all : Entity.Label.t -> string -> Migration.t list Lwt.t
  val upsert : Entity.Label.t -> string -> Migration.t -> unit Lwt.t
end

(* Common functions *)
let get_request table =
  let open Caqti_request.Infix in
  Format.sprintf
    {sql|
       SELECT
         namespace,
         version,
         dirty
       FROM %s
       WHERE namespace = ?
    |sql}
    table
  |> Caqti_type.(string ->? t3 string int bool)
;;

let get label table ~namespace =
  Service.find_opt label (get_request table) namespace
  |> Lwt.map (Option.map Migration.of_tuple)
;;

let get_all_request table =
  let open Caqti_request.Infix in
  Format.sprintf
    {sql|
       SELECT
         namespace,
         version,
         dirty
       FROM %s
    |sql}
    table
  |> Caqti_type.(unit ->* t3 string int bool)
;;

let get_all label table =
  Service.collect label (get_all_request table) ()
  |> Lwt.map (List.map Migration.of_tuple)
;;

module MariaDb : Sig = struct
  module Migration = Migration

  let create_request table =
    let open Caqti_request.Infix in
    Format.sprintf
      {sql|
        CREATE TABLE IF NOT EXISTS %s (
          namespace VARCHAR(128) NOT NULL,
          version INTEGER,
          dirty BOOL NOT NULL,
        PRIMARY KEY (namespace)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
      |sql}
      table
    |> Caqti_type.(unit ->. unit)
  ;;

  let create_table_if_not_exists label table =
    Service.exec label (create_request table) ()
  ;;

  let get = get
  let get_all = get_all

  let upsert_request table =
    let open Caqti_request.Infix in
    Format.sprintf
      {sql|
        INSERT INTO %s (
          namespace,
          version,
          dirty
        ) VALUES (
          ?,
          ?,
          ?
        ) ON DUPLICATE KEY UPDATE
          version = VALUES(version),
          dirty = VALUES(dirty)
      |sql}
      table
    |> Caqti_type.(t3 string int bool ->. unit)
  ;;

  let upsert label table state =
    Service.exec label (upsert_request table) (Migration.to_tuple state)
  ;;
end
