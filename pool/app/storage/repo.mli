val register_migration : unit -> unit
val register_cleaner : unit -> unit
val insert_file : Database.Label.t -> Entity.stored -> unit Lwt.t
val insert_blob : Database.Label.t -> id:string -> string -> unit Lwt.t
val get_file : Database.Label.t -> string -> Entity.stored option Lwt.t
val get_blob : Database.Label.t -> string -> string option Lwt.t
val update_file : Database.Label.t -> Entity.stored -> unit Lwt.t
val update_blob : Database.Label.t -> id:string -> string -> unit Lwt.t
val delete_file : Database.Label.t -> string -> unit Lwt.t
val delete_blob : Database.Label.t -> string -> unit Lwt.t
