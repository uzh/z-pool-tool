type role = Event.role =
  | Assistant
  | Experimenter
  | Recruiter
  | LocationManager
  | Operator

val equal_role : role -> role -> bool
val pp_role : Format.formatter -> role -> unit
val show_role : role -> string

type create = Event.create =
  { email : Pool_user.EmailAddress.t
  ; password : Pool_user.Password.t
  ; firstname : Pool_user.Firstname.t
  ; lastname : Pool_user.Lastname.t
  }

val equal_create : create -> create -> bool
val pp_create : Format.formatter -> create -> unit
val show_create : create -> string

type update = Event.update =
  { firstname : Pool_user.Firstname.t
  ; lastname : Pool_user.Lastname.t
  }

val equal_update : update -> update -> bool
val pp_update : Format.formatter -> update -> unit
val show_update : update -> string

type 'a person_event =
  | DetailsUpdated of 'a Entity.t * update
  | PasswordUpdated of
      'a Entity.t * Pool_user.Password.t * Pool_user.PasswordConfirmed.t
  | RoleUpdated of 'a Entity.t * role
  | Disabled of 'a Entity.t
  | Verified of 'a Entity.t

type event =
  | Created of role * create
  | AssistantEvents of Entity.assistant person_event
  | ExperimenterEvents of Entity.experimenter person_event
  | LocationManagerEvents of Entity.location_manager person_event
  | RecruiterEvents of Entity.recruiter person_event
  | OperatorEvents of Entity.operator person_event

val handle_person_event : Pool_database.Label.t -> 'a person_event -> unit Lwt.t
val handle_event : Pool_database.Label.t -> event -> unit Lwt.t
val equal_person_event : 'a person_event -> 'a person_event -> bool
val pp_person_event : Format.formatter -> 'a person_event -> unit
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit

type person = Entity.person =
  { user : Sihl_user.t
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }

val create_person : Sihl_user.t -> person
val equal_person : person -> person -> bool
val pp_person : Format.formatter -> person -> unit
val show_person : person -> string

type assistant = Entity.assistant
type experimenter = Entity.experimenter
type location_manager = Entity.location_manager
type recruiter = Entity.recruiter
type operator = Entity.operator

type 'a t = 'a Entity.t =
  | Assistant : person -> assistant t
  | Experimenter : person -> experimenter t
  | LocationManager : person -> location_manager t
  | Recruiter : person -> recruiter t
  | Operator : person -> operator t

(* Carries type information, is a type "witness" *)
type _ carrier =
  | AssistantC : assistant carrier
  | ExperimenterC : experimenter carrier
  | LocationManagerC : location_manager carrier
  | RecruiterC : recruiter carrier
  | OperatorC : operator carrier

val equal : 'person t -> 'person t -> bool
val pp : Format.formatter -> 'person t -> unit

type any_person = Entity.any_person = Any : 'a t -> any_person

val equal_any_person : any_person -> any_person -> bool
val pp_any_person : Format.formatter -> any_person -> unit
val user : 'person_function t -> Sihl_user.t

module Duplicate = Entity.Duplicate

val insert : Pool_database.Label.t -> 'a t -> unit Lwt.t

val find_by_email
  :  Pool_database.Label.t
  -> Pool_user.EmailAddress.t
  -> (any_person, Pool_common.Message.error) result Lwt.t

val find_by_user : 'a -> 'b
val user_is_admin : Pool_database.Label.t -> Sihl_user.t -> bool Lwt.t
val find_duplicates : 'a -> 'b
