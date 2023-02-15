module Database = Pool_database
module Id : module type of Pool_common.Id

module SmtpAuth : sig
  module Id : module type of Pool_common.Id
  module Label : Pool_common.Model.StringSig
  module Server : Pool_common.Model.StringSig
  module Port : Pool_common.Model.IntegerSig
  module Username : Pool_common.Model.StringSig
  module Password : Pool_common.Model.StringSig
  module Mechanism : Pool_common.Model.StringSig
  module Protocol : Pool_common.Model.StringSig

  type t =
    { id : Id.t
    ; label : Label.t
    ; server : Server.t
    ; port : Port.t
    ; username : Username.t option
    ; mechanism : Mechanism.t
    ; protocol : Protocol.t
    }

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
      }

    val create
      :  ?id:Id.t
      -> Label.t
      -> Server.t
      -> Port.t
      -> Username.t option
      -> Password.t option
      -> Mechanism.t
      -> Protocol.t
      -> (t, Pool_common.Message.error) result
  end

  val find
    :  Database.Label.t
    -> Id.t
    -> (t, Pool_common.Message.error) Lwt_result.t

  val find_by_label
    :  Database.Label.t
    -> (t, Pool_common.Message.error) Lwt_result.t

  val find_full_by_label
    :  Database.Label.t
    -> (Write.t, Pool_common.Message.error) Lwt_result.t
end

module Title : Pool_common.Model.StringSig
module Description : Pool_common.Model.StringSig

module Url : sig
  include Pool_common.Model.StringSig

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

    val create : string -> (t, Pool_common.Message.error) result
    val value : t -> string

    val schema
      :  unit
      -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
  end
end

module Icon : sig
  type t

  val value : t -> Pool_common.File.t
  val equal : t -> t -> bool
  val of_file : Pool_common.File.t -> t

  module Write : sig
    type t

    val create : string -> (t, Pool_common.Message.error) result
    val value : t -> string

    val schema
      :  unit
      -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
  end
end

module Logos : sig
  type t

  val value : t -> Pool_common.File.t list
  val equal : t -> t -> bool

  val schema
    :  unit
    -> ( Pool_common.Message.error
       , Pool_common.Id.t list )
       Pool_common.Utils.PoolConformist.Field.t

  val of_files : Pool_common.File.t list -> t
end

module PartnerLogos : sig
  type t

  val value : t -> Pool_common.File.t list
  val equal : t -> t -> bool

  val schema
    :  unit
    -> ( Pool_common.Message.error
       , Pool_common.Id.t list )
       Pool_common.Utils.PoolConformist.Field.t

  val of_files : Pool_common.File.t list -> t
end

module Maintenance : Pool_common.Model.BooleanSig
module Disabled : Pool_common.Model.BooleanSig

module LogoMapping : sig
  module LogoType : sig
    type t =
      | PartnerLogo
      | TenantLogo

    val of_string : string -> (t, Pool_common.Message.error) result
    val to_string : t -> string
    val all : t list
    val all_fields : Pool_common.Message.Field.t list
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
  ; description : Description.t
  ; url : Url.t
  ; database_label : Database.Label.t
  ; styles : Styles.t
  ; icon : Icon.t
  ; logos : Logos.t
  ; partner_logo : PartnerLogos.t
  ; maintenance : Maintenance.t
  ; disabled : Disabled.t
  ; default_language : Pool_common.Language.t
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
    ; description : Description.t
    ; url : Url.t
    ; database : Database.t
    ; styles : Styles.Write.t
    ; icon : Icon.Write.t
    ; maintenance : Maintenance.t
    ; disabled : Disabled.t
    ; default_language : Pool_common.Language.t
    ; created_at : Pool_common.CreatedAt.t
    ; updated_at : Pool_common.UpdatedAt.t
    }

  val create
    :  Title.t
    -> Description.t
    -> Url.t
    -> Database.t
    -> Styles.Write.t
    -> Icon.Write.t
    -> Pool_common.Language.t
    -> t

  val show : t -> string
end

type update =
  { title : Title.t
  ; description : Description.t
  ; url : Url.t
  ; disabled : Disabled.t
  ; default_language : Pool_common.Language.t
  }

type logo_mappings = LogoMapping.Write.t list

type event =
  | Created of Write.t [@equal equal]
  | LogosUploaded of logo_mappings
  | LogoDeleted of t * Pool_common.Id.t
  | DetailsEdited of Write.t * update
  | DatabaseEdited of Write.t * Database.t
  | SmtpCreated of SmtpAuth.Write.t
  | SmtpEdited of SmtpAuth.t
  | SmtpPasswordEdited of SmtpAuth.update_password
  | Destroyed of Id.t
  | ActivateMaintenance of Write.t
  | DeactivateMaintenance of Write.t

val handle_event : Database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val to_ctx : Database.Label.t -> (string * string) list
val find : Id.t -> (t, Pool_common.Message.error) Lwt_result.t
val find_full : Id.t -> (Write.t, Pool_common.Message.error) Lwt_result.t

val find_by_label
  :  Database.Label.t
  -> (t, Pool_common.Message.error) Lwt_result.t

val find_all : unit -> t list Lwt.t
val find_databases : unit -> Database.t list Lwt.t

val find_styles
  :  Database.Label.t
  -> (Styles.t, Pool_common.Message.error) Lwt_result.t

type handle_list_recruiters = unit -> Sihl_user.t list Lwt.t
type handle_list_tenants = unit -> t list Lwt.t

module Selection : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : Url.t -> Database.Label.t -> t
  val find_all : unit -> t list Lwt.t
  val url : t -> string
  val label : t -> Database.Label.t
end

module Guard : sig
  module Actor : sig
    val to_authorizable
      :  ?ctx:Guardian__Persistence.context
      -> t
      -> ( [> `Tenant ] Guard.Authorizable.t
         , Pool_common.Message.error )
         Lwt_result.t

    type t

    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module Target : sig
    val to_authorizable
      :  ?ctx:Guardian__Persistence.context
      -> t
      -> ( [> `Tenant ] Guard.AuthorizableTarget.t
         , Pool_common.Message.error )
         Lwt_result.t

    type t
  end
end

module Service : sig
  module Queue : Sihl.Contract.Queue.Sig

  module Email : sig
    module Smtp : sig
      type prepared =
        { sender : string
        ; recipients : Letters.recipient list
        ; subject : string
        ; body : Letters.body
        ; config : Letters.Config.t
        }

      val inbox : unit -> Sihl_email.t list
      val clear_inbox : unit -> unit
      val prepare : Database.Label.t -> Sihl_email.t -> prepared Lwt.t
    end

    module Job : sig
      val send : Sihl_email.t Sihl_queue.job
    end

    val remove_from_cache : Pool_database.Label.t -> unit
    val intercept : (Sihl_email.t -> unit Lwt.t) -> Sihl_email.t -> unit Lwt.t
    val send : Pool_database.Label.t -> Sihl_email.t -> unit Lwt.t
    val bulk_send : Pool_database.Label.t -> Sihl_email.t list -> unit Lwt.t
    val lifecycle : Sihl.Container.lifecycle
    val register : unit -> Sihl.Container.Service.t
  end
end
