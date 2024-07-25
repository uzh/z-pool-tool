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
val update : ?new_recipient:Pool_user.CellPhone.t -> t -> t

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
    val encode : t -> string
    val decode : string -> (t, Pool_message.Error.t) result
    val show_recipient : Pool_queue.Instance.t -> string

    val handle
      :  ?id:Pool_queue.Id.t
      -> Database.Label.t
      -> t
      -> (unit, Pool_message.Error.t) result Lwt.t

    val send : t Pool_queue.Job.t
  end

  val dispatch
    :  ?id:Pool_queue.Id.t
    -> ?new_recipient:Pool_user.CellPhone.t
    -> ?message_template:string
    -> ?job_ctx:Pool_queue.job_ctx
    -> Database.Label.t
    -> t
    -> unit Lwt.t
end

type job =
  { job : t
  ; id : Pool_queue.Id.t option
  ; message_template : string option
  ; job_ctx : Pool_queue.job_ctx option
  }

val equal_job : job -> job -> bool
val pp_job : Format.formatter -> job -> unit
val show_job : job -> string
val yojson_of_job : job -> Yojson.Safe.t
val job : job -> t
val id : job -> Pool_queue.Id.t option
val message_template : job -> string option
val job_ctx : job -> Pool_queue.job_ctx option

val create_job
  :  ?id:Pool_queue.Id.t
  -> ?message_template:string
  -> ?job_ctx:Pool_queue.job_ctx
  -> t
  -> job

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
  { job_id : Pool_queue.Id.t
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
  -> Pool_queue.Id.t
  -> delivery_report option Lwt.t

type event =
  | Sent of (job * Pool_user.CellPhone.t option)
  | BulkSent of job list
  | ReportCreated of delivery_report

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val handle_event : Database.Label.t -> event -> unit Lwt.t

val create_sent
  :  ?id:Pool_queue.Id.t
  -> ?message_template:string
  -> ?job_ctx:Pool_queue.job_ctx
  -> ?new_recipient:Pool_user.CellPhone.t
  -> t
  -> event

val sent : ?new_recipient:Pool_user.CellPhone.t -> job -> event
