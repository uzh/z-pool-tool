val register_migration : unit -> unit
val register_cleaner : unit -> unit
val insert_file : Database.Label.t -> Sihl.Contract.Storage.stored -> unit Lwt.t
val insert_blob : Database.Label.t -> id:string -> string -> unit Lwt.t
val get_file : Database.Label.t -> string -> Sihl.Contract.Storage.stored option Lwt.t
val get_blob : Database.Label.t -> string -> string option Lwt.t
val update_file : Database.Label.t -> Sihl.Contract.Storage.stored -> unit Lwt.t
val update_blob : Database.Label.t -> id:string -> string -> unit Lwt.t
val delete_file : Database.Label.t -> string -> unit Lwt.t
val delete_blob : Database.Label.t -> string -> unit Lwt.t
