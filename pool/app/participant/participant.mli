module RecruitmentChannel : sig
  type t =
    | Friend
    | Online
    | Lecture
    | Mailing

  val to_string : t -> string
  val schema : unit -> ('a, t) Conformist.Field.t
end

type t = Entity.t =
  { user : Sihl_user.t
  ; recruitment_channel : Entity.RecruitmentChannel.t
  ; terms_accepted_at : Common_user.TermsAccepted.t
  ; paused : Common_user.Paused.t
  ; disabled : Common_user.Disabled.t
  ; verified : Common_user.Verified.t
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }

val login : 'a -> email:'b -> password:'c -> 'd
val find_by_user : 'a -> 'b
val find_duplicates : 'a -> 'b

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
  | DetailsUpdated of Entity.t * update
  | PasswordUpdated of
      Entity.t * Common_user.Password.t * Common_user.PasswordConfirmed.t
  | Disabled of Entity.t
  | Verified of Entity.t

val handle_event : Pool_common.Database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
