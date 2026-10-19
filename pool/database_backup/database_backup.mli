(** Gzipped SQL dumps of the root database and every tenant database
    registered in it, written over {!Database.with_connection}. Restore one
    with [gunzip -c <file>.sql.gz | mariadb <database>].

    Each run writes one gzipped [<label>_<UTC timestamp>.sql.gz] per database
    into its own directory
    [<DATABASE_BACKUP_DIR>/<UTC timestamp>_<version>]. A run where a dump failed
    is renamed to [..._incomplete]. Meant to be run manually (see the
    [db.backup] command) before an update. *)

(** [DATABASE_BACKUP_DIR], defaults to [<ROOT_PATH>/backups] *)
val directory : unit -> string

module Run : sig
  val name : version:string -> Ptime.t -> string
  val is_run : string -> bool
  val version : string -> string option

  (** Names of all run directories within the given directory, oldest first *)
  val all : string -> string list
end

type outcome =
  { directory : string
  ; failed : (string * string) list (** database label, error *)
  }

module Dump : sig
  (** Default [max_allowed_packet]: the default --net-buffer-length of
      mariadb-dump *)
  val default_max_allowed_packet : int
end

(** Parses a size in bytes with an optional K, M or G suffix (e.g. [16M]) *)
val parse_size : string -> int option

(** Dumps a single database into [<directory>/<label>_<time>.sql.gz] (default
    [time]: now) and returns the file's path. All tables are read in one transaction (consistent snapshot).
    INSERT statements are split before they exceed [max_allowed_packet]; a
    larger row gets a statement of its own and is logged as a warning. *)
val dump
  :  ?max_allowed_packet:int
  -> ?time:Ptime.t
  -> directory:string
  -> Database.t
  -> (string, string) result Lwt.t

(** Dumps the given databases (see {!Database.Pool.registered}) into a new run
    directory. *)
val create
  :  ?max_allowed_packet:int
  -> ?directory:string
  -> version:string
  -> Database.t list
  -> outcome Lwt.t

(** Removes all backup runs and returns how many were removed. Only dump files
    are deleted; a run directory containing anything else is kept. *)
val remove_all : ?directory:string -> unit -> int
