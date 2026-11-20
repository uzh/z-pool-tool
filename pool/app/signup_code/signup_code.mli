val url_key : Pool_message.Field.t
val payload_url_key : Pool_message.Field.t

module Id : sig
  include module type of Pool_common.Id
end

module Code : sig
  include Pool_model.Base.StringSig
end

module Count : sig
  type t

  val value : t -> int
end

module WebhookUrl : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> (t, Pool_message.Error.t) result
  val value : t -> string
  val of_string : string -> t

  val schema
    :  Pool_message.Field.t
    -> unit
    -> (Pool_message.Error.t, t) Pool_conformist.Field.t
end

module Payload : sig
  include Pool_model.Base.StringSig
end

type t =
  { id : Id.t
  ; code : Code.t
  ; signup_count : Count.t
  ; verification_count : Count.t
  ; signed_up_webhook_url : WebhookUrl.t option
  ; email_verified_webhook_url : WebhookUrl.t option
  }

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val show : t -> string

val create
  :  ?id:Id.t
  -> ?signed_up_webhook_url:WebhookUrl.t
  -> ?email_verified_webhook_url:WebhookUrl.t
  -> Code.t
  -> t

module Webhook : sig
  module Trigger : sig
    type t =
      | SignedUp
      | EmailVerified

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  type t =
    { url : WebhookUrl.t
    ; trigger : Trigger.t
    ; code : Code.t
    ; payload : Payload.t option
    }

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val code_query_key : string
  val payload_query_key : string
  val request_uri : t -> Uri.t

  module Job : sig
    val encode : t -> string
    val decode : string -> (t, Pool_message.Error.t) result
    val show_recipient : Pool_queue.Instance.t -> string
    val send : t Pool_queue.Job.t
  end
end

val webhook : Webhook.Trigger.t -> ?payload:Payload.t -> t -> Webhook.t option

type event =
  | Created of t
  | Updated of t
  | SignedUp of Code.t
  | Verified of Code.t
  | WebhookTriggered of Webhook.t
  | WebhookResent of Webhook.t * Pool_queue.Id.t

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val handle_event : _ Database.ctx -> event -> unit Lwt.t

val signed_up_events
  :  ?signup_code:Code.t
  -> ?signup_webhook:Webhook.t
  -> unit
  -> event list

val verified_events
  :  ?signup_code:Code.t
  -> ?signup_webhook:Webhook.t
  -> unit
  -> event list

val find : _ Database.ctx -> Id.t -> (t, Pool_message.Error.t) Lwt_result.t
val find_by_code : _ Database.ctx -> Code.t -> t option Lwt.t

val find_webhook
  :  _ Database.ctx
  -> Webhook.Trigger.t
  -> ?payload:Payload.t
  -> Code.t
  -> Webhook.t option Lwt.t

val all : ?query:Query.t -> _ Database.ctx -> (t list * Query.t) Lwt.t
val column_code : Query.Column.t
val column_signup_count : Query.Column.t
val column_verification_count : Query.Column.t
val searchable_by : Query.Column.t list
val sortable_by : Query.Column.t list
val filterable_by : Query.Filter.human option
val default_sort : Query.Sort.t
val default_query : Query.t

module Access : sig
  val index : Guard.ValidationSet.t
  val create : Guard.ValidationSet.t
  val update : Id.t -> Guard.ValidationSet.t
end
