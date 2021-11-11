module Id = Pool_common.Id

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
  ; terms_accepted_at : Common_user.TermsAccepted.t
  ; paused : Common_user.Paused.t
  ; disabled : Common_user.Disabled.t
  ; verified : Common_user.Verified.t
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }

val login : 'a -> email:'b -> password:'c -> 'd
val insert : Pool_common.Database.Label.t -> t -> unit Lwt.t

val find
  :  Pool_common.Database.Label.t
  -> Pool_common.Id.t
  -> (t, Pool_common.Message.error) Result.t Lwt.t

val find_by_email
  :  Pool_common.Database.Label.t
  -> Common_user.Email.Address.t
  -> (t, Pool_common.Message.error) Result.t Lwt.t

val find_by_user
  :  Pool_common.Database.Label.t
  -> Sihl_user.t
  -> (t, Pool_common.Message.error) Result.t Lwt.t

val find_duplicates : 'a -> 'b
val has_terms_accepted : t -> bool Lwt.t

type create =
  { email : Common_user.Email.Address.t
  ; password : Common_user.Password.t
  ; firstname : Common_user.Firstname.t
  ; lastname : Common_user.Lastname.t
  ; recruitment_channel : RecruitmentChannel.t
  ; terms_accepted_at : Common_user.TermsAccepted.t
  }

type update = Event.update =
  { firstname : Common_user.Firstname.t
  ; lastname : Common_user.Lastname.t
  ; paused : Common_user.Paused.t
  }

type event =
  | Created of create
  | DetailsUpdated of t * update
  | PasswordUpdated of
      t * Common_user.Password.t * Common_user.PasswordConfirmed.t
  | EmailUnconfirmed of t
  | EmailConfirmed of t
  | AcceptTerms of t
  | Disabled of t
  | Verified of t

val handle_event : Pool_common.Database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
