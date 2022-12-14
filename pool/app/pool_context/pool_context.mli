type user =
  | Admin of Admin.t
  | Contact of Contact.t
  | Guest

val sexp_of_user : user -> Ppx_sexp_conv_lib.Sexp.t
val admin : Admin.t -> user
val contact : Contact.t -> user
val guest : user

module UserType : sig
  type t =
    | Admin
    | Contact
    | Guest

  val user_in : t list -> user -> bool
end

type t =
  { query_language : Pool_common.Language.t option
  ; language : Pool_common.Language.t
  ; database_label : Pool_database.Label.t
  ; message : Pool_common.Message.Collection.t option
  ; csrf : string
  ; user : user
  }

(* val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t *)
val show : t -> string
val pp : Format.formatter -> t -> unit
val find : Rock.Request.t -> (t, Pool_common.Message.error) result
val find_exn : Rock.Request.t -> t
val set : Rock.Request.t -> t -> Rock.Request.t
val find_contact : t -> (Contact.t, Pool_common.Message.error) result
val user_of_sihl_user : Pool_database.Label.t -> Sihl_user.t -> user Lwt.t

val create
  :  Pool_common.Language.t option
     * Pool_common.Language.t
     * Pool_database.Label.t
     * Pool_common.Message.Collection.t option
     * string
     * user
  -> t

val find_authenticatable
  :  t
  -> ( [> `Admin | `Contact ] Guard.Persistence.authorizable
     , Pool_common.Message.error )
     Lwt_result.t

module Tenant : sig
  type t =
    { tenant : Pool_tenant.t
    ; tenant_languages : Pool_common.Language.t list
    }

  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
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
val is_from_root : t -> bool
