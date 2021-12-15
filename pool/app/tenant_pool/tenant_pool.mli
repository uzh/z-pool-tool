module Database = Database_pool

module SmtpAuth : sig
  module Server : sig
    type t

    val value : t -> string
    val equal : t -> t -> bool
    val create : string -> (t, Pool_common.Message.error) result
    val schema : unit -> ('a, t) Conformist.Field.t
  end

  module Port : sig
    type t

    val value : t -> string
    val equal : t -> t -> bool
    val create : string -> (t, Pool_common.Message.error) result
    val schema : unit -> ('a, t) Conformist.Field.t
  end

  module Username : sig
    type t

    val value : t -> string
    val equal : t -> t -> bool
    val create : string -> (t, Pool_common.Message.error) result
    val schema : unit -> ('a, t) Conformist.Field.t
  end

  module Password : sig
    type t

    val equal : t -> t -> bool
    val create : string -> (t, Pool_common.Message.error) result
    val schema : unit -> ('a, t) Conformist.Field.t
  end

  module AuthenticationMethod : sig
    type t

    val value : t -> string
    val equal : t -> t -> bool
    val create : string -> (t, Pool_common.Message.error) result
    val schema : unit -> ('a, t) Conformist.Field.t
  end

  module Protocol : sig
    type t

    val value : t -> string
    val equal : t -> t -> bool
    val create : string -> (t, Pool_common.Message.error) result
    val schema : unit -> ('a, t) Conformist.Field.t
  end

  type t =
    { server : Server.t
    ; port : Port.t
    ; username : Username.t
    ; authentication_method : AuthenticationMethod.t
    ; protocol : Protocol.t
    }

  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool

  module Write : sig
    type t =
      { server : Server.t
      ; port : Port.t
      ; username : Username.t
      ; password : Password.t
      ; authentication_method : AuthenticationMethod.t
      ; protocol : Protocol.t
      }

    val create
      :  Server.t
      -> Port.t
      -> Username.t
      -> Password.t
      -> AuthenticationMethod.t
      -> Protocol.t
      -> (t, Pool_common.Message.error) result
  end
end

module Title : sig
  type t

  val value : t -> string
  val equal : t -> t -> bool
  val create : string -> (t, Pool_common.Message.error) result
  val schema : unit -> ('a, t) Conformist.Field.t
end

module Description : sig
  type t

  val value : t -> string
  val equal : t -> t -> bool
  val create : string -> (t, Pool_common.Message.error) result
  val schema : unit -> ('a, t) Conformist.Field.t
end

module Url : sig
  type t

  val value : t -> string
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val create : string -> (t, Pool_common.Message.error) result
  val schema : unit -> ('a, t) Conformist.Field.t
  val of_pool : Database.Label.t -> t Lwt.t
end

module Styles : sig
  type t

  val value : t -> Pool_common.File.t
  val equal : t -> t -> bool
  val id : t -> Pool_common.Id.t
  val mime_type : t -> Pool_common.File.Mime.t

  module Write : sig
    type t

    val create : string -> (t, Pool_common.Message.error) result
    val value : t -> string
    val schema : unit -> ('a, t) Conformist.Field.t
  end
end

module Icon : sig
  type t

  val value : t -> Pool_common.File.t
  val equal : t -> t -> bool

  module Write : sig
    type t

    val create : string -> (t, Pool_common.Message.error) result
    val value : t -> string
    val schema : unit -> ('a, t) Conformist.Field.t
  end
end

module Logos : sig
  type t

  val value : t -> Pool_common.File.t list
  val equal : t -> t -> bool
  val schema : unit -> ('a, Pool_common.Id.t list) Conformist.Field.t
end

module PartnerLogos : sig
  type t

  val value : t -> Pool_common.File.t list
  val equal : t -> t -> bool
  val schema : unit -> ('a, Pool_common.Id.t list) Conformist.Field.t
end

module Maintenance : sig
  type t

  val equal : t -> t -> bool
  val create : bool -> t
  val schema : unit -> ('a, t) Conformist.Field.t
end

module Disabled : sig
  type t

  val equal : t -> t -> bool
  val create : bool -> t
  val value : t -> bool
  val schema : unit -> ('a, t) Conformist.Field.t
end

module LogoMapping : sig
  module LogoType : sig
    type t =
      | PartnerLogo
      | TenantLogo

    val of_string : string -> (t, Pool_common.Message.error) result
    val to_string : t -> string
    val all : unit -> string list
  end

  module Write : sig
    type t =
      { id : Pool_common.Id.t
      ; tenant_id : Pool_common.Id.t
      ; asset_id : Pool_common.Id.t
      ; logo_type : LogoType.t
      }

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end
end

type t =
  { id : Pool_common.Id.t
  ; title : Title.t
  ; description : Description.t
  ; url : Url.t
  ; database_label : Database.Label.t
  ; smtp_auth : SmtpAuth.t
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

module Write : sig
  type t =
    { id : Pool_common.Id.t
    ; title : Title.t
    ; description : Description.t
    ; url : Url.t
    ; database : Database.t
    ; smtp_auth : SmtpAuth.Write.t
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
    -> SmtpAuth.Write.t
    -> Styles.Write.t
    -> Icon.Write.t
    -> Pool_common.Language.t
    -> t

  val show : t -> string
end

type smtp_auth_update =
  { server : SmtpAuth.Server.t
  ; port : SmtpAuth.Port.t
  ; username : SmtpAuth.Username.t
  ; authentication_method : SmtpAuth.AuthenticationMethod.t
  ; protocol : SmtpAuth.Protocol.t
  }

type update =
  { title : Title.t
  ; description : Description.t
  ; url : Url.t
  ; smtp_auth : smtp_auth_update
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
  | Destroyed of Pool_common.Id.t
  | ActivateMaintenance of Write.t
  | DeactivateMaintenance of Write.t

val handle_event : Database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val pool_to_ctx : Database.Label.t -> (string * string) list
val find : Pool_common.Id.t -> (t, Pool_common.Message.error) result Lwt.t

val find_full
  :  Pool_common.Id.t
  -> (Write.t, Pool_common.Message.error) result Lwt.t

val find_by_label
  :  Database.Label.t
  -> (t, Pool_common.Message.error) result Lwt.t

val find_all : unit -> t list Lwt.t
val find_databases : unit -> Database.t list Lwt.t

val find_styles
  :  Database.Label.t
  -> (Styles.t, Pool_common.Message.error) result Lwt.t

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
