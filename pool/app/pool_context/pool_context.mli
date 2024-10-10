type user =
  | Admin of Admin.t
  | Contact of Contact.t
  | Guest

val equal_user : user -> user -> bool
val pp_user : Format.formatter -> user -> unit
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
  ; database_label : Database.Label.t
  ; message : Pool_message.Collection.t option
  ; csrf : string
  ; user : user
  ; guardian : Guard.PermissionOnTarget.t list
  ; announcement : Announcement.t option
  }

val show : t -> string
val pp : Format.formatter -> t -> unit
val find : Rock.Request.t -> (t, Pool_message.Error.t) result
val find_exn : Rock.Request.t -> t
val set : Rock.Request.t -> t -> Rock.Request.t
val find_contact : t -> (Contact.t, Pool_message.Error.t) result
val context_user_of_user : Database.Label.t -> Pool_user.t -> user Lwt.t
val dashboard_path : ?guest:string -> user -> string

val create
  :  Pool_common.Language.t option
     * Pool_common.Language.t
     * Database.Label.t
     * Pool_message.Collection.t option
     * string
     * user
     * Guard.PermissionOnTarget.t list
     * Announcement.t option
  -> t

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
  val find : Rock.Request.t -> (t, Pool_message.Error.t) result
  val set : Rock.Request.t -> t -> Rock.Request.t
  val get_tenant_languages_exn : Rock.Request.t -> Pool_common.Language.t list
  val get_tenant_exn : Rock.Request.t -> Pool_tenant.t
  val text_messages_enabled : Rock.Request.t -> bool
end

val sexp_of_t : t -> Sexplib.Sexp.t
val is_from_root : t -> bool
val user_is_admin : user -> bool
val get_admin_user : user -> (Admin.t, Pool_message.Error.t) result
val get_user_id : user -> Pool_common.Id.t option

module Utils : sig
  val find_authorizable_opt
    :  ?admin_only:bool
    -> Database.Label.t
    -> user
    -> Guard.Actor.t option Lwt.t

  val find_authorizable
    :  ?admin_only:bool
    -> Database.Label.t
    -> user
    -> (Guard.Actor.t, Pool_message.Error.t) Lwt_result.t
end

module Logger : sig
  module Tags : sig
    val req : Sihl.Web.Request.t -> Logs.Tag.set
    val context : t -> Logs.Tag.set
  end
end
