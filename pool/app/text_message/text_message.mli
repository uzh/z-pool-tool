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

type job =
  { message : t
  ; message_history : Queue.History.create option
  ; resent : Pool_common.Id.t option
  }

val parse_job_json : string -> (job, Pool_message.Error.t) result
val yojson_of_job : job -> Yojson.Safe.t
val job_message_history : job -> Queue.History.create option
val create_job : ?message_history:Queue.History.create -> t -> job

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
    -> (Pool_tenant.GtxApiKey.t, Pool_message.Error.t) result Lwt.t

  module Job : sig
    val send : job Queue.job
  end

  val send : Database.Label.t -> job -> unit Lwt.t
end

type event =
  | Sent of job
  | BulkSent of job list

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val handle_event : Database.Label.t -> event -> unit Lwt.t
val sent : job -> event
val bulksent : job list -> event
