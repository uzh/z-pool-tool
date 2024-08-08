module Id = struct
  include Pool_common.Id
end

type t =
  { id : Id.t
  ; changes : Yojson.Safe.t
  ; model : Pool_message.Field.t
  ; user_uuid : Pool_common.Id.t
  ; created_at : Pool_common.CreatedAt.t
  }

module type RecordSig = sig
  type t

  val model : Pool_message.Field.t
  val yojson_of_t : t -> Yojson.Safe.t
end

module type TSig = sig
  type record

  val model : Pool_message.Field.t

  val create
    :  Database.Label.t
    -> ?id:Id.t
    -> user_uuid:Pool_common.Id.t
    -> before:record
    -> after:record
    -> unit
    -> unit Lwt.t

  val find_all : ?query:Query.t -> Database.Label.t -> (t list * Query.t) Lwt.t
end
