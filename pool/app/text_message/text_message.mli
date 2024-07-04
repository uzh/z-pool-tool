module Content : sig
  type t

  val of_string : string -> t
  val value : t -> string
end

type t =
  { recipient : Pool_user.CellPhone.t
  ; sender : Pool_tenant.Title.t
  ; text : Content.t
  }

val create : Pool_user.CellPhone.t -> Pool_tenant.Title.t -> Content.t -> t
val update : ?new_recipient:Pool_user.CellPhone.t -> t -> t

val render_and_create
  :  Pool_user.CellPhone.t
  -> Pool_tenant.Title.t
  -> string * (string * string) list
  -> t

module Service : sig
  val register : unit -> Sihl.Container.Service.t

  val test_api_key
    :  tags:Logs.Tag.set
    -> Pool_tenant.GtxApiKey.t
    -> Pool_user.CellPhone.t
    -> Pool_tenant.Title.t
    -> (Pool_tenant.GtxApiKey.t, Pool_message.Error.t) Lwt_result.t

  module Job : sig
    val encode : t -> string
    val decode : string -> (t, Pool_message.Error.t) result

    val handle
      :  Database.Label.t
      -> t
      -> (unit, Pool_message.Error.t) result Lwt.t

    val send : t Pool_queue.Job.t
  end

  val dispatch
    :  ?id:Pool_queue.Id.t
    -> ?new_recipient:Pool_user.CellPhone.t
    -> ?message_template:string
    -> ?mappings:Pool_queue.mappings
    -> Database.Label.t
    -> t
    -> unit Lwt.t
end

type job =
  { job : t
  ; id : Pool_queue.Id.t option
  ; message_template : string option
  ; mappings : Pool_queue.mappings option
  }

val equal_job : job -> job -> bool
val pp_job : Format.formatter -> job -> unit
val show_job : job -> string
val yojson_of_job : job -> Yojson.Safe.t
val job : job -> t
val id : job -> Pool_queue.Id.t option
val message_template : job -> string option
val mappings : job -> Pool_queue.mappings option

val create_job
  :  ?id:Pool_queue.Id.t
  -> ?message_template:string
  -> ?mappings:Pool_queue.mappings
  -> t
  -> job

type event =
  | Sent of (job * Pool_user.CellPhone.t option)
  | BulkSent of job list

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val handle_event : Database.Label.t -> event -> unit Lwt.t

val create_sent
  :  ?id:Pool_queue.Id.t
  -> ?message_template:string
  -> ?mappings:Pool_queue.mappings
  -> ?new_recipient:Pool_user.CellPhone.t
  -> t
  -> event

val sent : ?new_recipient:Pool_user.CellPhone.t -> job -> event
