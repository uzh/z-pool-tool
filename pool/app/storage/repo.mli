val register_migration : unit -> unit
val register_cleaner : unit -> unit
val insert_file : _ Database.ctx -> Sihl.Contract.Storage.stored -> unit Lwt.t
val insert_blob : _ Database.ctx -> id:string -> string -> unit Lwt.t
val get_file : _ Database.ctx -> string -> Sihl.Contract.Storage.stored option Lwt.t
val get_blob : _ Database.ctx -> string -> string option Lwt.t
val update_file : _ Database.ctx -> Sihl.Contract.Storage.stored -> unit Lwt.t
val update_blob : _ Database.ctx -> id:string -> string -> unit Lwt.t
val delete_file : _ Database.ctx -> string -> unit Lwt.t
val delete_blob : _ Database.ctx -> string -> unit Lwt.t
