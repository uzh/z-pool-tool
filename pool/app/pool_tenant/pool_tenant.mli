module Title : Pool_model.Base.StringSig
module Description : Pool_model.Base.StringSig
module GtxApiKey : Pool_model.Base.StringSig

module Id : sig
  include Pool_model.Base.IdSig

  val to_common : t -> Pool_common.Id.t
  val of_common : Pool_common.Id.t -> t
end

module Url : sig
  include Pool_model.Base.StringSig

  val of_pool : Database.Label.t -> t Lwt.t
end

module Styles : sig
  type t

  val value : t -> Pool_common.File.t
  val equal : t -> t -> bool
  val id : t -> Pool_common.Id.t
  val mime_type : t -> Pool_common.File.Mime.t
  val create : Pool_common.File.t -> t

  module Write : sig
    type t

    val create : string -> (t, Pool_message.Error.t) result
    val value : t -> string
    val schema : unit -> (Pool_message.Error.t, t) Pool_conformist.Field.t
  end
end

module Icon : sig
  type t

  val value : t -> Pool_common.File.t
  val equal : t -> t -> bool
  val of_file : Pool_common.File.t -> t

  module Write : sig
    type t

    val create : string -> (t, Pool_message.Error.t) result
    val value : t -> string
    val schema : unit -> (Pool_message.Error.t, t) Pool_conformist.Field.t
  end
end

module Logos : sig
  type t

  val value : t -> Pool_common.File.t list
  val equal : t -> t -> bool

  val schema
    :  unit
    -> (Pool_message.Error.t, Pool_common.Id.t list) Pool_conformist.Field.t

  val of_files : Pool_common.File.t list -> t
end

module PartnerLogos : sig
  type t

  val value : t -> Pool_common.File.t list
  val equal : t -> t -> bool

  val schema
    :  unit
    -> (Pool_message.Error.t, Pool_common.Id.t list) Pool_conformist.Field.t

  val of_files : Pool_common.File.t list -> t
end

module LogoMapping : sig
  module LogoType : sig
    type t =
      | PartnerLogo
      | TenantLogo

    val of_string : string -> (t, Pool_message.Error.t) result
    val to_string : t -> string
    val all : t list
    val all_fields : Pool_message.Field.t list
  end

  module Write : sig
    type t =
      { id : Pool_common.Id.t
      ; tenant_id : Id.t
      ; asset_id : Pool_common.Id.t
      ; logo_type : LogoType.t
      }

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end
end

type t =
  { id : Id.t
  ; title : Title.t
  ; description : Description.t option
  ; url : Url.t
  ; database_label : Database.Label.t
  ; styles : Styles.t option
  ; icon : Icon.t option
  ; logos : Logos.t
  ; partner_logo : PartnerLogos.t
  ; status : Database.Status.t
  ; default_language : Pool_common.Language.t
  ; text_messages_enabled : bool
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }

val id : t -> Id.t
val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
val pp : Format.formatter -> t -> unit
val equal : t -> t -> bool

module Write : sig
  type t =
    { id : Id.t
    ; title : Title.t
    ; description : Description.t option
    ; url : Url.t
    ; database_label : Database.Label.t
    ; gtx_api_key : GtxApiKey.t option
    ; styles : Styles.Write.t option
    ; icon : Icon.Write.t option
    ; default_language : Pool_common.Language.t
    ; created_at : Pool_common.CreatedAt.t
    ; updated_at : Pool_common.UpdatedAt.t
    }

  val create
    :  Title.t
    -> Description.t option
    -> Url.t
    -> Database.Label.t
    -> Styles.Write.t option
    -> Icon.Write.t option
    -> Pool_common.Language.t
    -> t

  val show : t -> string
  val database_label : t -> Database.Label.t
end

type update =
  { title : Title.t
  ; description : Description.t option
  ; url : Url.t
  ; status : Database.Status.t option
  ; default_language : Pool_common.Language.t
  ; styles : Styles.Write.t option
  ; icon : Icon.Write.t option
  }

val file_fields : Pool_message.Field.t list
val find : Id.t -> (t, Pool_message.Error.t) Lwt_result.t
val find_full : Id.t -> (Write.t, Pool_message.Error.t) Lwt_result.t
val find_by_label : Database.Label.t -> (t, Pool_message.Error.t) Lwt_result.t
val find_by_url : Url.t -> (t, Pool_message.Error.t) Lwt_result.t
val find_all : unit -> t list Lwt.t

val find_gtx_api_key_by_label
  :  Database.Label.t
  -> (GtxApiKey.t, Pool_message.Error.t) Lwt_result.t

val create_public_url : Url.t -> string -> string
val clear_cache : unit -> unit

type handle_list_recruiters = unit -> Pool_user.t list Lwt.t
type handle_list_tenants = unit -> t list Lwt.t
type logo_mappings = LogoMapping.Write.t list

type event =
  | Created of (Write.t * Database.t) [@equal equal]
  | LogosUploaded of logo_mappings
  | LogoDeleted of t * Pool_common.Id.t
  | DetailsEdited of Write.t * update
  | DatabaseEdited of Write.t * Database.t
  | ActivateMaintenance of Write.t
  | DeactivateMaintenance of Write.t
  | GtxApiKeyUpdated of Write.t * GtxApiKey.t
  | GtxApiKeyRemoved of Write.t

val handle_event : Database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string

module Guard : sig
  module Actor : sig
    val to_authorizable
      :  ?ctx:(string * string) list
      -> t
      -> (Guard.Actor.t, Pool_message.Error.t) Lwt_result.t

    type t

    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module Target : sig
    val to_authorizable
      :  ?ctx:(string * string) list
      -> t
      -> (Guard.Target.t, Pool_message.Error.t) Lwt_result.t

    type t
  end

  module Access : sig
    val index : Guard.ValidationSet.t
    val create : Guard.ValidationSet.t
    val read : Id.t -> Guard.ValidationSet.t
    val update : Id.t -> Guard.ValidationSet.t
    val delete : Id.t -> Guard.ValidationSet.t
  end
end
