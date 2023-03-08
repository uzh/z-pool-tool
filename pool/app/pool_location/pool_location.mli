module Address : sig
  module Mail : sig
    module Institution : sig
      include Pool_common.Model.StringSig
    end

    module Room : sig
      include Pool_common.Model.StringSig
    end

    module Building : sig
      include Pool_common.Model.StringSig
    end

    module Street : sig
      include Pool_common.Model.StringSig
    end

    module Zip : sig
      include Pool_common.Model.StringSig
    end

    module City : sig
      include Pool_common.Model.StringSig
    end

    type t =
      { institution : Institution.t option
      ; room : Room.t
      ; building : Building.t option
      ; street : Street.t
      ; zip : Zip.t
      ; city : City.t
      }

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string

    val create
      :  string option
      -> string
      -> string option
      -> string
      -> string
      -> string
      -> (t, Pool_common.Message.error) result

    val command
      :  Institution.t option
      -> Room.t
      -> Building.t option
      -> Street.t
      -> Zip.t
      -> City.t
      -> t

    val schema
      :  unit
      -> ( Pool_common.Message.error
         , Institution.t option
           -> Room.t
           -> Building.t option
           -> Street.t
           -> Zip.t
           -> City.t
           -> t
         , t )
         Pool_common.Utils.PoolConformist.t
  end

  type t =
    | Virtual
    | Physical of Mail.t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val physical : Mail.t -> t
  val virtual_ : t
end

module Mapping : sig
  module Id : sig
    include Pool_common.Model.IdSig
  end

  module Label : sig
    type t =
      | Direction
      | AdditionalInformation

    val all : t list
    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
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

  type file_base =
    { label : Label.t
    ; language : Pool_common.Language.t
    ; asset_id : Pool_common.Id.t
    }

  val equal_file_base : file_base -> file_base -> bool
  val pp_file_base : Format.formatter -> file_base -> unit
  val show_file_base : file_base -> string

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
  include Pool_common.Model.IdSig
end

module Name : sig
  include Pool_common.Model.StringSig
end

module Description : sig
  include Pool_common.Model.StringSig
end

module Link : sig
  include Pool_common.Model.StringSig
end

module Status : sig
  type t =
    | Active
    | Maintenance
    | Closed

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val init : t

  val schema
    :  unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t

  val all : t list
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

val contact_file_path : Id.t -> Mapping.file -> string
val admin_file_path : Id.t -> Mapping.file -> string
val to_string : Pool_common.Language.t -> t -> string

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
  | Created of t
  | FileUploaded of Mapping.Write.file
  | Updated of t * update
  | FileDeleted of Mapping.Id.t

val created : t -> event
val fileuploaded : Mapping.Write.file -> event
val updated : t -> update -> event
val filedeleted : Mapping.Id.t -> event
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val handle_event : Pool_tenant.Database.Label.t -> event -> unit Lwt.t

module Repo : sig
  module Id : sig
    val t : Id.t Caqti_type.t
  end
end

val find
  :  Pool_database.Label.t
  -> Id.t
  -> (t, Pool_common.Message.error) Lwt_result.t

val find_all : Pool_database.Label.t -> t list Lwt.t

val find_file_storage_blob
  :  Pool_database.Label.t
  -> Pool_common.Repo.Id.t
  -> (Sihl_storage.stored, Entity.Message.error) result Lwt.t

val default_values : t list

module Guard : sig
  module Target : sig
    val to_authorizable
      :  ?ctx:Guardian__Persistence.context
      -> t
      -> ( [> `Location ] Guard.AuthorizableTarget.t
         , Pool_common.Message.error )
         Lwt_result.t

    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module FileTarget : sig
    val to_authorizable
      :  ?ctx:Guardian__Persistence.context
      -> Mapping.file
      -> ( [> `LocationFile ] Guard.AuthorizableTarget.t
         , Pool_common.Message.error )
         Lwt_result.t

    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end
end
