module Token : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> t
  val value : t -> string
end

module VerifiedAt : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val value : t -> Ptime.t
  val create : Ptime.t -> t
  val create_now : unit -> t
end

type email_unverified =
  { address : Pool_user.EmailAddress.t
  ; user : Sihl_user.t
  ; token : Token.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }

type email_verified =
  { address : Pool_user.EmailAddress.t
  ; user : Sihl_user.t
  ; verified_at : VerifiedAt.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }

type unverified
type verified

val equal_email_unverified : email_unverified -> email_unverified -> bool
val equal_email_verified : email_verified -> email_verified -> bool
val pp_email_unverified : Format.formatter -> email_unverified -> unit
val pp_email_verified : Format.formatter -> email_verified -> unit
val show_email_unverified : email_unverified -> string
val show_email_verified : email_verified -> string

type _ t =
  | Unverified : email_unverified -> unverified t
  | Verified : email_verified -> verified t

val equal : 'email t -> 'email t -> bool
val pp : Format.formatter -> 'email t -> unit
val show : 'state t -> string
val token : unverified t -> string
val verify : unverified t -> verified t
val address : 'email t -> Pool_user.EmailAddress.t
val user_id : 'email t -> Pool_common.Id.t
val user_is_confirmed : 'email t -> bool
val create : Pool_user.EmailAddress.t -> Sihl_user.t -> Token.t -> unverified t

val find_unverified_by_user
  :  Database.Label.t
  -> Pool_common.Id.t
  -> (unverified t, Pool_message.Error.t) result Lwt.t

val find_verified_by_user
  :  Database.Label.t
  -> Pool_common.Id.t
  -> (verified t, Pool_message.Error.t) result Lwt.t

val find_unverified_by_address
  :  Database.Label.t
  -> Pool_user.EmailAddress.t
  -> (unverified t, Pool_message.Error.t) result Lwt.t

val delete_unverified_by_user
  :  Database.Label.t
  -> Pool_common.Id.t
  -> unit Lwt.t

val create_token : Database.Label.t -> Pool_user.EmailAddress.t -> Token.t Lwt.t

module SmtpAuth : sig
  module Id : module type of Pool_common.Id
  module Label : Pool_model.Base.StringSig
  module Server : Pool_model.Base.StringSig
  module Port : Pool_model.Base.IntegerSig
  module Username : Pool_model.Base.StringSig
  module Password : Pool_model.Base.StringSig

  module RepoEntity : sig
    module Id : sig
      val t : Id.t Caqti_type.t
    end
  end

  module Mechanism : sig
    type t =
      | PLAIN
      | LOGIN

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
    val t_of_yojson : Yojson.Safe.t -> t
    val yojson_of_t : t -> Yojson.Safe.t
    val read : string -> t
    val all : t list
    val schema : unit -> (Pool_message.Error.t, t) Pool_conformist.Field.t
  end

  module Protocol : sig
    type t =
      | STARTTLS
      | SSL_TLS

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
    val t_of_yojson : Yojson.Safe.t -> t
    val yojson_of_t : t -> Yojson.Safe.t
    val read : string -> t
    val all : t list
    val schema : unit -> (Pool_message.Error.t, t) Pool_conformist.Field.t
  end

  module Default : sig
    include Pool_model.Base.BooleanSig
  end

  type t =
    { id : Id.t
    ; label : Label.t
    ; server : Server.t
    ; port : Port.t
    ; username : Username.t option
    ; mechanism : Mechanism.t
    ; protocol : Protocol.t
    ; default : Default.t
    }

  val id : t -> Id.t
  val label : t -> Label.t

  type smtp = t

  type update_password =
    { id : Id.t
    ; password : Password.t option
    }

  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool

  module Write : sig
    type t =
      { id : Id.t
      ; label : Label.t
      ; server : Server.t
      ; port : Port.t
      ; username : Username.t option
      ; password : Password.t option
      ; mechanism : Mechanism.t
      ; protocol : Protocol.t
      ; default : Default.t
      }

    val to_entity : t -> smtp

    val create
      :  ?id:Id.t
      -> Label.t
      -> Server.t
      -> Port.t
      -> Username.t option
      -> Password.t option
      -> Mechanism.t
      -> Protocol.t
      -> Default.t
      -> (t, Pool_message.Error.t) result
  end

  val find : Database.Label.t -> Id.t -> (t, Pool_message.Error.t) Lwt_result.t
  val find_by_label : Database.Label.t -> Label.t -> t option Lwt.t

  val find_full
    :  Database.Label.t
    -> Id.t
    -> (Write.t, Pool_message.Error.t) Lwt_result.t

  val find_default : Database.Label.t -> (t, Pool_message.Error.t) Lwt_result.t
  val find_default_opt : Database.Label.t -> t option Lwt.t
  val find_all : Database.Label.t -> t list Lwt.t
  val find_by : Query.t -> Database.Label.t -> (t list * Query.t) Lwt.t
  val column_label : Query.Column.t
  val column_smtp_server : Query.Column.t
  val column_smtp_username : Query.Column.t
  val column_smtp_mechanism : Query.Column.t
  val column_smtp_protocol : Query.Column.t
  val column_smtp_default_account : Query.Column.t
  val filterable_by : Query.Filter.human option
  val default_query : Query.t
  val searchable_by : Query.Column.t list
  val sortable_by : Query.Column.t list
end

type job =
  { email : Sihl.Contract.Email.t
  ; smtp_auth_id : SmtpAuth.Id.t option
  ; message_history : Queue.History.create option
  ; resent : Pool_common.Id.t option
  }

val parse_job_json : string -> (job, Pool_message.Error.t) result
val yojson_of_job : job -> Yojson.Safe.t
val job_message_history : job -> Queue.History.create option

val create_job
  :  ?smtp_auth_id:SmtpAuth.Id.t
  -> ?message_history:Queue.History.create
  -> Sihl.Contract.Email.t
  -> job

module Service : sig
  module Queue : Sihl.Contract.Queue.Sig

  module Cache : sig
    val clear : unit -> unit
  end

  module Smtp : sig
    type prepared =
      { sender : string
      ; reply_to : string
      ; recipients : Letters.recipient list
      ; subject : string
      ; body : Letters.body
      ; config : Letters.Config.t
      }

    val inbox : unit -> Sihl_email.t list
    val clear_inbox : unit -> unit

    val prepare
      :  Database.Label.t
      -> ?smtp_auth_id:SmtpAuth.Id.t
      -> Sihl_email.t
      -> prepared Lwt.t
  end

  module Job : sig
    val send : job Sihl_queue.job
  end

  val default_sender_of_pool
    :  Database.Label.t
    -> Pool_user.EmailAddress.t Lwt.t

  val intercept_prepare : job -> (job, Pool_message.Error.t) result
  val dispatch : Database.Label.t -> job -> unit Lwt.t
  val dispatch_all : Database.Label.t -> job list -> unit Lwt.t
  val lifecycle : Sihl.Container.lifecycle
  val register : unit -> Sihl.Container.Service.t

  val test_smtp_config
    :  Database.Label.t
    -> SmtpAuth.Write.t
    -> Pool_user.EmailAddress.t
    -> (unit, Pool_message.Error.t) Lwt_result.t
end

module Guard : sig
  module Access : sig
    module Smtp : sig
      val index : Guard.ValidationSet.t
      val create : Guard.ValidationSet.t
      val read : SmtpAuth.Id.t -> Guard.ValidationSet.t
      val update : SmtpAuth.Id.t -> Guard.ValidationSet.t
      val delete : SmtpAuth.Id.t -> Guard.ValidationSet.t
    end
  end
end

type verification_event =
  | Created of Pool_user.EmailAddress.t * Token.t * Pool_common.Id.t
  | EmailVerified of unverified t

val handle_verification_event
  :  Database.Label.t
  -> verification_event
  -> unit Lwt.t

val equal_verification_event : verification_event -> verification_event -> bool
val pp_verification_event : Format.formatter -> verification_event -> unit

type event =
  | Sent of job
  | BulkSent of job list
  | SmtpCreated of SmtpAuth.Write.t
  | SmtpEdited of SmtpAuth.t
  | SmtpDeleted of SmtpAuth.Id.t
  | SmtpPasswordEdited of SmtpAuth.update_password

val handle_event : Database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val verification_event_name : verification_event -> string
val sent : job -> event
val bulksent : job list -> event
