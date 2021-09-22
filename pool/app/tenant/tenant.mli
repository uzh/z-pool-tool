module SmtpAuth : sig
  module Server : sig
    type t

    val equal : t -> t -> bool
    val create : string -> (t, string) result
    val schema : unit -> ('a, t) Conformist.Field.t
  end

  module Port : sig
    type t

    val equal : t -> t -> bool
    val create : string -> (t, string) result
    val schema : unit -> ('a, t) Conformist.Field.t
  end

  module Username : sig
    type t

    val equal : t -> t -> bool
    val create : string -> (t, string) result
    val schema : unit -> ('a, t) Conformist.Field.t
  end

  module AuthenticationMethod : sig
    type t

    val equal : t -> t -> bool
    val create : string -> (t, string) result
    val schema : unit -> ('a, t) Conformist.Field.t
  end

  module Protocol : sig
    type t

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

  val create
    :  Server.t
    -> Port.t
    -> Username.t
    -> AuthenticationMethod.t
    -> Protocol.t
    -> (t, string) result
end

module Title : sig
  type t

  val equal : t -> t -> bool
  val create : string -> (t, string) result
  val schema : unit -> ('a, t) Conformist.Field.t
end

module Description : sig
  type t

  val equal : t -> t -> bool
  val create : string -> (t, string) result
  val schema : unit -> ('a, t) Conformist.Field.t
end

module Url : sig
  type t

  val equal : t -> t -> bool
  val create : string -> (t, string) result
  val schema : unit -> ('a, t) Conformist.Field.t
end

module DatabaseUrl : sig
  type t

  val equal : t -> t -> bool
  val create : string -> (t, string) result
  val schema : unit -> ('a, t) Conformist.Field.t
end

module Styles : sig
  type t

  val equal : t -> t -> bool
  val create : string -> (t, string) result
  val schema : unit -> ('a, t) Conformist.Field.t
end

module Icon : sig
  type t

  val equal : t -> t -> bool
  val create : string -> (t, string) result
  val schema : unit -> ('a, t) Conformist.Field.t
end

module Logos : sig
  type t

  val equal : t -> t -> bool
  val create : string -> (t, string) result
  val schema : unit -> ('a, t) Conformist.Field.t
end

module PartnerLogo : sig
  type t

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
  val schema : unit -> ('a, t) Conformist.Field.t
end

type t =
  { id : Pool_common.Id.t
  ; title : Title.t
  ; description : Description.t
  ; url : Url.t
  ; database_url : DatabaseUrl.t
  ; smtp_auth : SmtpAuth.t
  ; styles : Styles.t
  ; icon : Icon.t
  ; logos : Logos.t
  ; partner_logos : PartnerLogo.t
  ; maintenance : Maintenance.t
  ; disabled : Disabled.t
  ; default_language : Settings.Language.t
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }

val create
  :  Title.t
  -> Description.t
  -> Url.t
  -> DatabaseUrl.t
  -> SmtpAuth.t
  -> Styles.t
  -> Icon.t
  -> Logos.t
  -> PartnerLogo.t
  -> Settings.Language.t
  -> t

module StatusReport : sig
  type t

  val equal : t -> t -> bool
end

type create =
  { title : Title.t
  ; description : Description.t
  ; url : Url.t
  ; database_url : DatabaseUrl.t
  ; smtp_auth : SmtpAuth.t
  ; styles : Styles.t
  ; icon : Icon.t
  ; logos : Logos.t
  ; partner_logos : PartnerLogo.t
  ; default_language : Settings.Language.t
  }

type update =
  { title : Title.t
  ; description : Description.t
  ; url : Url.t
  ; database_url : DatabaseUrl.t
  ; smtp_auth : SmtpAuth.t
  ; styles : Styles.t
  ; icon : Icon.t
  ; logos : Logos.t
  ; partner_logos : PartnerLogo.t
  ; disabled : Disabled.t
  ; default_language : Settings.Language.t
  }

type event =
  | Added of create
  | Edited of t * update
  | Destroyed of Pool_common.Id.t
  | Disabled of t
  | Enabled of t
  | ActivateMaintenance of t
  | DeactivateMaintenance of t
  | OperatorAssigned of t * Admin.operator Admin.t
  | OperatorDivested of t * Admin.operator Admin.t
  | StatusReportGenerated of unit

val handle_event : event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val find_by_id : string -> (t, string) result Lwt.t
val find_by_participant : 'a -> 'b
val find_by_user : 'a -> 'b
val find_all : unit -> (t list, string) Result.t Lwt.t

type handle_list_recruiters = unit -> Sihl_user.t list Lwt.t
type handle_list_tenants = unit -> t list Lwt.t

(* MONITORING AND MANAGEMENT *)

(* The system should proactively report degraded health to operators *)
type generate_status_report = StatusReport.t Lwt.t
