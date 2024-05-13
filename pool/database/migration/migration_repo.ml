open CCFun.Infix

module Migration : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val version : t -> int
  val namespace : t -> string
  val create : namespace:string -> t
  val mark_dirty : t -> t
  val mark_clean : t -> t
  val increment : t -> t
  val steps_to_apply : 'a * 'b list -> t -> 'a * 'b list
  val dirty : t -> bool
  val t : t Caqti_type.t
end = struct
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

  let t =
    let open Caqti_encoders in
    let decode (namespace, (version, (dirty, ()))) =
      Ok { namespace; version; dirty }
    in
    let encode t : ('a Data.t, string) result =
      Ok Data.[ namespace t; version t; dirty t ]
    in
    custom ~encode ~decode Schema.(Caqti_type.[ string; int; bool ])
  ;;
end

(* Common functions *)
open Caqti_request.Infix

let get_request =
  Format.asprintf
    {sql|
       SELECT
         namespace,
         version,
         dirty
       FROM %s
       WHERE namespace = ?
    |sql}
  %> (Caqti_type.string ->? Migration.t)
;;

let get label table ~namespace =
  Service.find_opt label (get_request table) namespace
;;

let get_all_request =
  Format.asprintf
    {sql|
       SELECT
         namespace,
         version,
         dirty
       FROM %s
    |sql}
  %> (Caqti_type.unit ->* Migration.t)
;;

let get_all label table = Service.collect label (get_all_request table) ()

let create_request =
  Format.asprintf
    {sql|
        CREATE TABLE IF NOT EXISTS %s (
          namespace VARCHAR(128) NOT NULL,
          version INTEGER,
          dirty BOOL NOT NULL,
        PRIMARY KEY (namespace)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
      |sql}
  %> Caqti_type.(unit ->. unit)
;;

let create_table_if_not_exists label table =
  Service.exec label (create_request table) ()
;;

let get = get
let get_all = get_all

let upsert_request =
  Format.asprintf
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
  %> (Migration.t ->. Caqti_type.unit)
;;

let upsert label = upsert_request %> Service.exec label
