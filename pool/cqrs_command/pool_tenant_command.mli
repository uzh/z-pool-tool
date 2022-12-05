module Conformist = Pool_common.Utils.PoolConformist
module Id = Pool_common.Id
module User = Pool_user
module File = Pool_common.File

val create_logo_mappings
  :  Id.t list
  -> Pool_tenant.Write.t
  -> Pool_tenant.LogoMapping.LogoType.t
  -> Pool_tenant.LogoMapping.Write.t list

module Create : sig
  type t =
    { title : Pool_tenant.Title.t
    ; description : Pool_tenant.Description.t
    ; url : Pool_tenant.Url.t
    ; database_url : Pool_database.Url.t
    ; database_label : Pool_database.Label.t
    ; smtp_auth_server : Pool_tenant.SmtpAuth.Server.t
    ; smtp_auth_port : Pool_tenant.SmtpAuth.Port.t
    ; smtp_auth_username : Pool_tenant.SmtpAuth.Username.t
    ; smtp_auth_password : Pool_tenant.SmtpAuth.Password.t
    ; smtp_auth_authentication_method :
        Pool_tenant.SmtpAuth.AuthenticationMethod.t
    ; smtp_auth_protocol : Pool_tenant.SmtpAuth.Protocol.t
    ; styles : Pool_tenant.Styles.Write.t
    ; icon : Pool_tenant.Icon.Write.t
    ; default_language : Pool_common.Language.t
    ; tenant_logos : Id.t list
    ; partner_logos : Id.t list
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Conformist.error_msg) result

  val decode : (string * string list) list -> (t, Conformist.error_msg) result
  val effects : Guard.Authorizer.effect list
end

module EditDetails : sig
  type t =
    { title : Pool_tenant.Title.t
    ; description : Pool_tenant.Description.t
    ; url : Pool_tenant.Url.t
    ; smtp_auth_server : Pool_tenant.SmtpAuth.Server.t
    ; smtp_auth_port : Pool_tenant.SmtpAuth.Port.t
    ; smtp_auth_username : Pool_tenant.SmtpAuth.Username.t
    ; smtp_auth_authentication_method :
        Pool_tenant.SmtpAuth.AuthenticationMethod.t
    ; smtp_auth_protocol : Pool_tenant.SmtpAuth.Protocol.t
    ; disabled : Pool_tenant.Disabled.t
    ; default_language : Pool_common.Language.t
    ; tenant_logos : Id.t list option
    ; partner_logos : Id.t list option
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> Pool_tenant.Write.t
    -> t
    -> (Pool_event.t list, Conformist.error_msg) result

  val decode : (string * string list) list -> (t, Conformist.error_msg) result
  val effects : Pool_tenant.Write.t -> Guard.Authorizer.effect list
end

module EditDatabase : sig
  type t =
    { database_url : Pool_database.Url.t
    ; database_label : Pool_database.Label.t
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> Pool_tenant.Write.t
    -> t
    -> (Pool_event.t list, Conformist.error_msg) result

  val decode : (string * string list) list -> (t, Conformist.error_msg) result

  val effects
    :  Pool_database.Label.t
    -> (Guard.Authorizer.effect list, Conformist.error_msg) Lwt_result.t
end

module DestroyLogo : sig
  val handle
    :  ?tags:Logs.Tag.set
    -> Pool_tenant.t
    -> Id.t
    -> (Pool_event.t list, Conformist.error_msg) result

  val effects : Guard.Authorizer.effect list
end

module Destroy : sig
  type t = { tenant_id : string }

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Conformist.error_msg) result

  val effects : t -> Guard.Authorizer.effect list
end
