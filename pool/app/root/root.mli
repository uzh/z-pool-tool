type create = Event.create =
  { email : Common_user.EmailAddress.t
  ; password : Common_user.Password.t
  ; firstname : Common_user.Firstname.t
  ; lastname : Common_user.Lastname.t
  }

type t

val equal_create : create -> create -> bool
val pp_create : Format.formatter -> create -> unit
val show_create : create -> string

type event =
  | Created of create
  | Disabled of t
  | Enabled of t

val handle_event : Database_pool.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val user : t -> Sihl_user.t
val find_all : unit -> t list Lwt.t
val find : Pool_common.Id.t -> (t, Pool_common.Message.error) result Lwt.t
