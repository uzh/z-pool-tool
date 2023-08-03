module Id : module type of Pool_common.Id

module Job : sig
  type t =
    | GuardianCacheCleared
    | I18nPageUpdated
    | SmtpAccountUpdated
    | TenantDatabaseAdded of Pool_database.Label.t
    | TenantDatabaseUpdated of Pool_database.Label.t
    | TenantDatabaseDeleted of Pool_database.Label.t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val read : string -> t
  val of_string : string -> (t, Pool_common.Message.error) result
  val to_string : t -> string
end

type t =
  { id : Id.t
  ; job : Job.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }

val equal : t -> t -> Ppx_deriving_runtime.bool
val pp : Ppx_deriving_runtime.Format.formatter -> t -> Ppx_deriving_runtime.unit
val show : t -> Ppx_deriving_runtime.string
val create : ?id:Id.t -> Job.t -> t

module EventLog : sig
  module ServiceIdentifier : sig
    include Pool_common.Model.StringSig

    val get : ?identifier:string -> unit -> t
  end

  module Status : sig
    type t =
      | Failed
      | Successful

    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
    val t_of_yojson : Yojson.Safe.t -> t
    val yojson_of_t : t -> Yojson.Safe.t
    val equal : t -> t -> bool
  end

  module Message : sig
    include Pool_common.Model.StringSig
  end

  type t
end

type event = Created of t

val created : t -> event
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val find_pending : EventLog.ServiceIdentifier.t -> t list Lwt.t
val handle_event : event -> unit Lwt.t
val handle_system_event : t -> unit Lwt.t

module Service : sig
  val run : ?identifier:string -> unit -> unit Lwt.t
  val register : ?identifier:string -> unit -> Sihl.Container.Service.t
  val register_worker : unit -> Sihl.Container.Service.t
end
