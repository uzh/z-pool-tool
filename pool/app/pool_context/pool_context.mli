type t =
  { query_language : Pool_common.Language.t option
  ; language : Pool_common.Language.t
  ; tenant_db : Pool_tenant.Database.Label.t
  ; message : Pool_common.Message.Collection.t option
  ; csrf : string
  }

val create
  :  Pool_common.Language.t option
     * Pool_common.Language.t
     * Pool_tenant.Database.Label.t
     * Pool_common.Message.Collection.t option
     * string
  -> t

val find : Rock.Request.t -> (t, Pool_common.Message.error) result
val find_exn : Rock.Request.t -> t
val set : Rock.Request.t -> t -> Rock.Request.t

module Tenant : sig
  type t =
    { tenant : Pool_tenant.t
    ; tenant_languages : Pool_common.Language.t list
    }

  val create : Pool_tenant.t -> Pool_common.Language.t list -> t
  val key : t Rock.Context.key
  val find : Rock.Request.t -> (t, Pool_common.Message.error) result
  val set : Rock.Request.t -> t -> Rock.Request.t

  val get_tenant_languages
    :  Rock.Request.t
    -> (Pool_common.Language.t list, Pool_common.Message.error) result
end
