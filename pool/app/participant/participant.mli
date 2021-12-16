module RecruitmentChannel : sig
  type t =
    | Friend
    | Online
    | Lecture
    | Mailing

  val to_string : t -> string
  val schema : unit -> ('a, t) Conformist.Field.t
  val all : unit -> string list
end

type t =
  { user : Sihl_user.t
  ; recruitment_channel : RecruitmentChannel.t
  ; terms_accepted_at : Pool_user.TermsAccepted.t
  ; paused : Pool_user.Paused.t
  ; disabled : Pool_user.Disabled.t
  ; verified : Pool_user.Verified.t
  ; firstname_version : Pool_common.Version.t
  ; lastname_version : Pool_common.Version.t
  ; paused_version : Pool_common.Version.t
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }

val id : t -> Pool_common.Id.t
val firstname : t -> Pool_user.Firstname.t
val lastname : t -> Pool_user.Lastname.t
val fullname : t -> string
val email_address : t -> Pool_user.EmailAddress.t
val version_selector : t -> string -> Pool_common.Version.t option
val login : 'a -> email:'b -> password:'c -> 'd
val insert : Database_pool.Label.t -> t -> unit Lwt.t

val find
  :  Database_pool.Label.t
  -> Pool_common.Id.t
  -> (t, Pool_common.Message.error) result Lwt.t

val find_by_email
  :  Database_pool.Label.t
  -> Pool_user.EmailAddress.t
  -> (t, Pool_common.Message.error) result Lwt.t

val find_by_user
  :  Database_pool.Label.t
  -> Sihl_user.t
  -> (t, Pool_common.Message.error) result Lwt.t

val find_duplicates : 'a -> 'b
val has_terms_accepted : Database_pool.Label.t -> t -> bool Lwt.t

type create =
  { email : Pool_user.EmailAddress.t
  ; password : Pool_user.Password.t
  ; firstname : Pool_user.Firstname.t
  ; lastname : Pool_user.Lastname.t
  ; recruitment_channel : RecruitmentChannel.t
  ; terms_accepted_at : Pool_user.TermsAccepted.t
  }

type update = Event.update =
  { firstname : Pool_user.Firstname.t
  ; lastname : Pool_user.Lastname.t
  ; paused : Pool_user.Paused.t
  }

type event =
  | Created of create
  | FirstnameUpdated of t * Pool_user.Firstname.t
  | LastnameUpdated of t * Pool_user.Lastname.t
  | PausedUpdated of t * Pool_user.Paused.t
  | EmailUpdated of t * Pool_user.EmailAddress.t
  | PasswordUpdated of
      t
      * Pool_user.Password.t
      * Pool_user.Password.t
      * Pool_user.PasswordConfirmed.t
  | EmailUnconfirmed of t
  | EmailConfirmed of t
  | TermsAccepted of t
  | Disabled of t
  | Verified of t

val created : create -> event
val firstnameupdated : t -> Pool_user.Firstname.t -> event
val lastnameupdated : t -> Pool_user.Lastname.t -> event
val pausedupdated : t -> Pool_user.Paused.t -> event
val handle_event : Database_pool.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
