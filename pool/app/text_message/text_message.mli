module Content : sig
  type t

  val of_string : string -> t
  val value : t -> string
end

type t =
  { recipient : Pool_user.CellPhone.t
  ; sender : Pool_tenant.GtxSender.t
  ; text : Content.t
  }

val create : Pool_user.CellPhone.t -> Pool_tenant.GtxSender.t -> Content.t -> t

type job =
  { message : t
  ; message_history : Queue.History.create option
  ; resent : Queue.Id.t option
  }

val parse_job_json : string -> (job, Pool_message.Error.t) result
val yojson_of_job : job -> Yojson.Safe.t
val job_message_history : job -> Queue.History.create option
val create_job : ?message_history:Queue.History.create -> t -> job

val render_and_create
  :  Pool_user.CellPhone.t
  -> Pool_tenant.GtxSender.t
  -> string * (string * string) list
  -> t

module Service : sig
  val register : unit -> Sihl.Container.Service.t

  val test_api_key
    :  tags:Logs.Tag.set
    -> Pool_tenant.GtxApiKey.t
    -> Pool_user.CellPhone.t
    -> Pool_tenant.GtxSender.t
    -> ( Pool_tenant.GtxApiKey.t * Pool_tenant.GtxSender.t
         , Pool_message.Error.t )
         Lwt_result.t

  module Job : sig
    val send : job Queue.Job.t
  end

  val send : Database.Label.t -> job -> unit Lwt.t
end

module DlrMask : sig
  type t =
    | Delivered
    | NonDelivered
    | Expired
    | Unknown

  val of_int : int -> t
  val to_human : t -> string
end

type delivery_report =
  { job_id : Queue.Id.t
  ; raw : string
  ; from : string
  ; to_ : string
  ; message_id : string
  ; dlr_mask : int
  ; error_code : int
  ; error_message : string
  ; submit_date : Pool_model.Base.Ptime.t
  ; done_date : Pool_model.Base.Ptime.t
  ; plmn : string
  ; country : string
  ; sms_cost : float
  }

val find_report_by_queue_id
  :  Database.Label.t
  -> Queue.Id.t
  -> delivery_report option Lwt.t

type event =
  | Sent of job
  | BulkSent of job list
  | ReportCreated of delivery_report

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val handle_event : Database.Label.t -> event -> unit Lwt.t
val sent : job -> event
val bulksent : job list -> event
