type user =
  | Admin of Service.User.t
  | Contact of Contact.t
  | Root of Service.User.t

val admin : Service.User.t -> user
val contact : Contact.t -> user
val root : Service.User.t -> user

type t =
  { query_language : Pool_common.Language.t option
  ; language : Pool_common.Language.t
  ; tenant_db : Pool_tenant.Database.Label.t
  ; message : Pool_common.Message.Collection.t option
  ; csrf : string
  ; user : user option
  }

val create
  :  Pool_common.Language.t option
     * Pool_common.Language.t
     * Pool_tenant.Database.Label.t
     * Pool_common.Message.Collection.t option
     * string
     * user option
  -> t

val find : Rock.Request.t -> (t, Pool_common.Message.error) result
val find_exn : Rock.Request.t -> t
val set : Rock.Request.t -> t -> Rock.Request.t
val find_contact : t -> (Contact.t, Pool_common.Message.error) result

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

val sexp_of_t : t -> Sexplib.Sexp.t
val show_log : t -> string
