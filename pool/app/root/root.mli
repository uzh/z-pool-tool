type create = Event.create =
  { email : Pool_user.EmailAddress.t
  ; password : Pool_user.Password.t
  ; firstname : Pool_user.Firstname.t
  ; lastname : Pool_user.Lastname.t
  }

type t

val equal_create : create -> create -> bool
val pp_create : Format.formatter -> create -> unit
val show_create : create -> string

type event =
  | Created of create
  | Disabled of t
  | Enabled of t

val handle_event : Pool_database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val user : t -> Sihl_user.t
val find_all : unit -> t list Lwt.t
val find : Pool_common.Id.t -> (t, Pool_common.Message.error) result Lwt.t
