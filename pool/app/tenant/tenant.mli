module File = Pool_common.File

module SmtpAuth : sig
  module Server : sig
    type t

    val value : t -> string
    val equal : t -> t -> bool
    val create : string -> (t, string) result
    val schema : unit -> ('a, t) Conformist.Field.t
  end

  module Port : sig
    type t

    val value : t -> string
    val equal : t -> t -> bool
    val create : string -> (t, string) result
    val schema : unit -> ('a, t) Conformist.Field.t
  end

  module Username : sig
    type t

    val value : t -> string
    val equal : t -> t -> bool
    val create : string -> (t, string) result
    val schema : unit -> ('a, t) Conformist.Field.t
  end

  module Password : sig
    type t

    val equal : t -> t -> bool
    val create : string -> (t, string) result
    val schema : unit -> ('a, t) Conformist.Field.t
  end

  module AuthenticationMethod : sig
    type t

    val value : t -> string
    val equal : t -> t -> bool
    val create : string -> (t, string) result
    val schema : unit -> ('a, t) Conformist.Field.t
  end

  module Protocol : sig
    type t

    val value : t -> string
    val equal : t -> t -> bool
    val create : string -> (t, string) result
    val schema : unit -> ('a, t) Conformist.Field.t
  end

  type t =
    { server : Server.t
    ; port : Port.t
    ; username : Username.t
    ; authentication_method : AuthenticationMethod.t
    ; protocol : Protocol.t
    }

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
      -> (t, string) result
  end
end

module Title : sig
  type t

  val value : t -> string
  val equal : t -> t -> bool
  val create : string -> (t, string) result
  val schema : unit -> ('a, t) Conformist.Field.t
end

module Description : sig
  type t

  val value : t -> string
  val equal : t -> t -> bool
  val create : string -> (t, string) result
  val schema : unit -> ('a, t) Conformist.Field.t
end

module Url : sig
  type t

  val value : t -> string
  val equal : t -> t -> bool
  val create : string -> (t, string) result
  val schema : unit -> ('a, t) Conformist.Field.t
end

module Styles : sig
  type t

  val value : t -> File.t
  val equal : t -> t -> bool

  module Write : sig
    type t

    val value : t -> string
    val schema : unit -> ('a, t) Conformist.Field.t
  end
end

module Icon : sig
  type t

  val value : t -> File.t
  val equal : t -> t -> bool

  module Write : sig
    type t

    val value : t -> string
    val schema : unit -> ('a, t) Conformist.Field.t
  end
end

module Logos : sig
  type t

  val value : t -> File.t list
  val equal : t -> t -> bool
end

module PartnerLogos : sig
  type t

  val value : t -> string
  val equal : t -> t -> bool
  val create : string -> (t, string) result
  val schema : unit -> ('a, t) Conformist.Field.t
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
  module Write : sig
    type t =
      { id : Pool_common.Id.t
      ; tenant_uuid : Pool_common.Id.t
      ; asset_uuid : Pool_common.Id.t
      ; logo_type : [ `PartnerLogo | `TenantLogo ]
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
  ; database_label : Pool_common.Database.Label.t
  ; smtp_auth : SmtpAuth.t
  ; styles : Styles.t
  ; icon : Icon.t
  ; logos : Logos.t
  ; partner_logos : PartnerLogos.t
  ; maintenance : Maintenance.t
  ; disabled : Disabled.t
  ; default_language : Settings.Language.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }

module Write : sig
  type t =
    { id : Pool_common.Id.t
    ; title : Title.t
    ; description : Description.t
    ; url : Url.t
    ; database : Pool_common.Database.t
    ; smtp_auth : SmtpAuth.Write.t
    ; styles : Styles.Write.t
    ; icon : Icon.Write.t
    ; partner_logos : PartnerLogos.t
    ; maintenance : Maintenance.t
    ; disabled : Disabled.t
    ; default_language : Settings.Language.t
    ; created_at : Pool_common.CreatedAt.t
    ; updated_at : Pool_common.UpdatedAt.t
    }

  val create
    :  Title.t
    -> Description.t
    -> Url.t
    -> Pool_common.Database.t
    -> SmtpAuth.Write.t
    -> Styles.Write.t
    -> Icon.Write.t
    -> PartnerLogos.t
    -> Settings.Language.t
    -> t

  val show : t -> string
end

module StatusReport : sig
  type t

  val equal : t -> t -> bool
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
  ; partner_logos : PartnerLogos.t
  ; disabled : Disabled.t
  ; default_language : Settings.Language.t
  }

type logo_mappings = LogoMapping.Write.t list

type event =
  | Created of Write.t [@equal equal]
  | LogosUploaded of logo_mappings
  | LogoDeleted of t * Pool_common.Id.t
  | DetailsEdited of Write.t * update
  | DatabaseEdited of Write.t * Pool_common.Database.t
  | Destroyed of Pool_common.Id.t
  | ActivateMaintenance of Write.t
  | DeactivateMaintenance of Write.t
  | OperatorAssigned of Pool_common.Id.t * Admin.operator Admin.t
  | OperatorDivested of Pool_common.Id.t * Admin.operator Admin.t
  | StatusReportGenerated of unit

val handle_event : Pool_common.Database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val find : Pool_common.Id.t -> (t, string) result Lwt.t
val find_full : Pool_common.Id.t -> (Write.t, string) result Lwt.t
val find_by_participant : 'a -> 'b
val find_by_user : 'a -> 'b
val find_all : unit -> (t list, string) Result.t Lwt.t

val find_databases
  :  unit
  -> (Pool_common.Database.t list, string) Result.result Lwt.t

type handle_list_recruiters = unit -> Sihl_user.t list Lwt.t
type handle_list_tenants = unit -> t list Lwt.t

module Selection : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : Url.t -> Pool_common.Database.Label.t -> t
  val find_all : unit -> (t list, string) result Lwt.t
  val url : t -> string
  val label : t -> Pool_common.Database.Label.t
end

(* MONITORING AND MANAGEMENT *)

(* The system should proactively report degraded health to operators *)
type generate_status_report = StatusReport.t Lwt.t

(* Logo Mappings *)
val stringify_logo_type : [ `PartnerLogo | `TenantLogo ] -> string

val logo_type_of_string
  :  string
  -> ([> `PartnerLogo | `TenantLogo ], string) result
