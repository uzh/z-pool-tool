module Address : sig
  module Mail : sig
    module Room : sig
      include Pool_common.Utils.BaseSig

      val value : t -> string
    end

    module Building : sig
      include Pool_common.Utils.BaseSig

      val value : t -> string
    end

    module Street : sig
      include Pool_common.Utils.BaseSig

      val value : t -> string
    end

    module Zip : sig
      include Pool_common.Utils.BaseSig

      val value : t -> string
    end

    module City : sig
      include Pool_common.Utils.BaseSig

      val value : t -> string
    end

    type t =
      { room : Room.t
      ; building : Building.t option
      ; street : Street.t
      ; zip : Zip.t
      ; city : City.t
      }

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string

    val create
      :  string
      -> string option
      -> string
      -> string
      -> string
      -> (t, Pool_common.Message.error) result

    val command
      :  Room.t
      -> Building.t option
      -> Street.t
      -> Zip.t
      -> City.t
      -> t

    val schema
      :  unit
      -> ( Pool_common.Message.error
         , Room.t -> Building.t option -> Street.t -> Zip.t -> City.t -> t
         , t )
         Pool_common.Utils.PoolConformist.t
  end

  type t =
    | Address of Mail.t
    | Virtual

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val address : Mail.t -> t
  val virtual_ : t
end

module Mapping : sig
  module Id : sig
    include Pool_common.Utils.BaseSig

    val create : unit -> t
    val value : t -> string
    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
    val t_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t
  end

  module Label : sig
    type t =
      | Direction
      | AdditionalInformation

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val t_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t
    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
    val create : string -> (t, Pool_common.Message.error) result

    val schema
      :  unit
      -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
  end

  type file =
    { id : Id.t
    ; label : Label.t
    ; language : Pool_common.Language.t
    ; file : Pool_common.File.t
    }

  val equal_file : file -> file -> bool
  val pp_file : Format.formatter -> file -> unit
  val show_file : file -> string

  type create =
    { label : Label.t
    ; language : Pool_common.Language.t
    ; file : Pool_common.File.t
    }

  val equal_create : create -> create -> bool
  val pp_create : Format.formatter -> create -> unit
  val show_create : create -> string

  val create
    :  ?id:Id.t
    -> string
    -> Pool_common.Language.t
    -> Pool_common.File.t
    -> (file, Pool_common.Utils.PoolConformist.error_msg) result

  module Write : sig
    type file =
      { id : Id.t
      ; label : Label.t
      ; language : Pool_common.Language.t
      ; asset_id : Pool_common.Id.t
      ; location_id : Pool_common.Id.t
      }

    val equal_file : file -> file -> bool
    val pp_file : Format.formatter -> file -> unit
    val show_file : file -> string
    val file_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> file
    val sexp_of_file : file -> Ppx_sexp_conv_lib.Sexp.t

    val create
      :  ?id:Id.t
      -> Label.t
      -> Pool_common.Language.t
      -> Pool_common.Id.t
      -> Pool_common.Id.t
      -> file
  end
end

module Id : sig
  include Pool_common.Utils.BaseSig

  val create : unit -> t
  val value : t -> string
  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
  val t_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t
end

module Name : sig
  include Pool_common.Utils.BaseSig

  val value : t -> string
end

module Description : sig
  include Pool_common.Utils.BaseSig

  val value : t -> string
end

module Link : sig
  include Pool_common.Utils.BaseSig

  val value : t -> string
end

module Status : sig
  type t =
    | Active
    | Maintenance
    | Closed

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
  val init : t

  val schema
    :  unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

type t =
  { id : Id.t
  ; name : Name.t
  ; description : Description.t option
  ; address : Address.t
  ; link : Link.t option
  ; status : Status.t
  ; files : Mapping.file list
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val show : t -> string

val create
  :  ?id:Id.t
  -> string
  -> string option
  -> Address.t
  -> string option
  -> Status.t
  -> Mapping.file list
  -> (t, Pool_common.Message.error) result

type update =
  { name : Name.t
  ; description : Description.t option
  ; address : Address.t
  ; link : Link.t option
  ; status : Status.t
  }

val equal_update : update -> update -> bool
val pp_update : Format.formatter -> update -> unit
val show_update : update -> string

type event =
  | Created of t * Mapping.Write.file list
  | FilesUploaded of t * Mapping.Write.file list
  | Updated of t * update
  | FileDeleted of Mapping.file

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val handle_event : Pool_tenant.Database.Label.t -> event -> unit Lwt.t

val find
  :  Pool_database.Label.t
  -> Id.t
  -> (t Lwt.t, Pool_common.Message.error) Lwt_result.t

val default_values : t list
