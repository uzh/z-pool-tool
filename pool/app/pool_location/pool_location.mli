module Address : sig
  module Mail : sig
    module Institution : sig
      include Pool_model.Base.StringSig
    end

    module Room : sig
      include Pool_model.Base.StringSig
    end

    module Building : sig
      include Pool_model.Base.StringSig
    end

    module Street : sig
      include Pool_model.Base.StringSig
    end

    module Zip : sig
      include Pool_model.Base.StringSig
    end

    module City : sig
      include Pool_model.Base.StringSig
    end

    type t =
      { institution : Institution.t option
      ; room : Room.t option
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
      -> string option
      -> string option
      -> string
      -> string
      -> string
      -> (t, Pool_message.Error.t) result

    val command
      :  Institution.t option
      -> Room.t option
      -> Building.t option
      -> Street.t
      -> Zip.t
      -> City.t
      -> t

    val schema
      :  unit
      -> ( Pool_message.Error.t
           , Institution.t option
             -> Room.t option
             -> Building.t option
             -> Street.t
             -> Zip.t
             -> City.t
             -> t
           , t )
           Pool_conformist.t
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
    include Pool_model.Base.IdSig

    val to_common : t -> Pool_common.Id.t
    val of_common : Pool_common.Id.t -> t
  end

  module Label : sig
    type t =
      | Direction
      | AdditionalInformation

    val all : t list
    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val create : string -> (t, Pool_message.Error.t) result
    val schema : unit -> (Pool_message.Error.t, t) Pool_conformist.Field.t
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
    ; asset_id : Id.t
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
    -> (file, Pool_message.Error.t) result

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
  include Pool_model.Base.IdSig

  val to_common : t -> Pool_common.Id.t
end

module Name : sig
  include Pool_model.Base.StringSig
end

module Description : sig
  type t

  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
  val show : t -> string
  val field_name : Pool_common.Language.t -> string
  val find_opt : Pool_common.Language.t -> t -> string option

  val create
    :  Pool_common.Language.t list
    -> (Pool_common.Language.t * string) list
    -> (t, Pool_message.Error.t) Result.t

  val value : t -> (Pool_common.Language.t * string) list
end

module Link : sig
  include Pool_model.Base.StringSig
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
  val schema : unit -> (Pool_message.Error.t, t) Pool_conformist.Field.t
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
  -> Description.t option
  -> Address.t
  -> string option
  -> Status.t
  -> Mapping.file list
  -> (t, Pool_message.Error.t) result

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
val handle_event : Database.Label.t -> event -> unit Lwt.t

module Repo : sig
  module Id : sig
    val t : Id.t Caqti_type.t
  end

  module Name : sig
    val t : Name.t Caqti_type.t
  end

  val sql_select_columns : string list
  val t : Repo_entity.t Caqti_type.t
  val to_entity : Repo_entity.t -> Entity_file_mapping.file list -> t
  val of_entity : t -> Repo_entity.t
end

val find : Database.Label.t -> Id.t -> (t, Pool_message.Error.t) Lwt_result.t
val find_all : Database.Label.t -> t list Lwt.t
val find_by : Query.t -> Database.Label.t -> (t list * Query.t) Lwt.t

val find_location_file
  :  Database.Label.t
  -> Pool_common.Repo.Id.t
  -> (Mapping.file, Pool_message.Error.t) Lwt_result.t

val search
  :  ?conditions:string
  -> ?dyn:Database.Dynparam.t
  -> ?exclude:Id.t list
  -> ?joins:string
  -> ?limit:int
  -> Database.Label.t
  -> string
  -> (Id.t * Name.t) list Lwt.t

val search_multiple_by_id
  :  Database.Label.t
  -> Id.t list
  -> (Id.t * Name.t) list Lwt.t

val find_targets_grantable_by_admin
  :  ?exclude:Id.t list
  -> Database.Label.t
  -> Admin.t
  -> string
  -> (Id.t * Name.t) list Lwt.t

val default_values : t list

module Human : sig
  val link_with_default : default:string -> t -> string
  val detailed : Pool_common.Language.t -> t -> string
  val description : Pool_common.Language.t -> t -> string option
end

module Statistics : sig
  module ExperimentCount : sig
    include Pool_model.Base.IntegerSig
  end

  module AssignmentCount : sig
    include Pool_model.Base.IntegerSig
  end

  module ShowUpCount : sig
    include Pool_model.Base.IntegerSig
  end

  module NoShowCount : sig
    include Pool_model.Base.IntegerSig
  end

  module ParticipationCount : sig
    include Pool_model.Base.IntegerSig
  end

  type t

  val experiment_count : t -> ExperimentCount.t
  val assignment_count : t -> AssignmentCount.t
  val showup_count : t -> ShowUpCount.t
  val noshow_count : t -> NoShowCount.t
  val participation_count : t -> ParticipationCount.t
  val create : ?year:int -> Database.Label.t -> Id.t -> t Lwt.t
  val year_select : Database.Label.t -> int list Lwt.t
end

module Guard : sig
  module Target : sig
    val to_authorizable
      :  ?ctx:(string * string) list
      -> t
      -> (Guard.Target.t, Pool_message.Error.t) Lwt_result.t

    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module FileTarget : sig
    val to_authorizable
      :  ?ctx:(string * string) list
      -> Mapping.file
      -> (Guard.Target.t, Pool_message.Error.t) Lwt_result.t

    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module Access : sig
    val index : Guard.ValidationSet.t
    val create : Guard.ValidationSet.t
    val read : ?model:Role.Target.t -> Id.t -> Guard.ValidationSet.t
    val update : ?model:Role.Target.t -> Id.t -> Guard.ValidationSet.t
    val delete : ?model:Role.Target.t -> Id.t -> Guard.ValidationSet.t

    module File : sig
      val index : Guard.ValidationSet.t
      val create : Guard.ValidationSet.t
      val read : Mapping.Id.t -> Guard.ValidationSet.t
      val update : Mapping.Id.t -> Guard.ValidationSet.t
      val delete : Mapping.Id.t -> Guard.ValidationSet.t
    end
  end
end

val column_name : Query.Column.t
val column_description : Query.Column.t
val default_query : Query.t
val filterable_by : Query.Filter.human option
val searchable_by : Query.Column.t list
val sortable_by : Query.Column.t list

module VersionHistory : Changelog.TSig with type record = t
