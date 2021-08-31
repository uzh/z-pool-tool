module Email = Entity_email

module Password = struct
  type t = string [@@deriving eq, show]
end

module PasswordConfirmed = struct
  type t = string [@@deriving eq, show]
end

module Firstname = struct
  type t = string [@@deriving eq, show]
end

module Lastname = struct
  type t = string [@@deriving eq, show]
end

module Paused = struct
  type t = bool [@@deriving eq, show]

  let t = Caqti_type.bool
end

module Disabled = struct
  type t = bool [@@deriving eq, show]

  let t = Caqti_type.bool
end

module TermsAccepted = struct
  type t = Ptime.t [@@deriving eq, show]

  let t = Caqti_type.ptime
end

module Verified = struct
  type t = Ptime.t [@@deriving eq, show]

  let t = Caqti_type.ptime
end

module RecruitmentChannel = struct
  type t =
    | Friend
    | Online
    | Lecture
    | Mailing
  [@@deriving eq, show]

  let of_string = function
    | "friend" -> Ok Friend
    | "online" -> Ok Online
    | "lecture" -> Ok Lecture
    | "mailing" -> Ok Mailing
    | _ -> Error "Invalid recruitment channel provided"
  ;;

  let to_string = function
    | Friend -> "friend"
    | Online -> "online"
    | Lecture -> "lecture"
    | Mailing -> "mailing"
  ;;

  let t =
    Caqti_type.(
      custom
        ~encode:(fun m -> m |> to_string |> Result.ok)
        ~decode:of_string
        string)
  ;;
end

type person =
  { user : Sihl_user.t
        [@equal fun m k -> String.equal m.Sihl_user.id k.Sihl_user.id]
  ; recruitment_channel : RecruitmentChannel.t
  ; terms_accepted_at : TermsAccepted.t
  ; paused : Paused.t
  ; disabled : Disabled.t
  ; verified : Verified.t
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }
[@@deriving eq, show]

(* TODO hide private constructors if possible *)
(* Don't use these private constructors *)
(* They are needed so the typechecker understands they are disjoint *)
type participant = private XParticipantP
type assistant = private XAssistantP
type experimenter = private XExperimenterP
type location_manager = private XLocationManagerP
type recruiter = private XRecruiterP
type operator = private XOperatorP

type _ t =
  | Participant : person -> participant t
  | Assistant : person -> assistant t
  | Experimenter : person -> experimenter t
  | LocationManager : person -> location_manager t
  | Recruiter : person -> recruiter t
  | Operator : person -> operator t

(* Carries type information, is a type "witness" *)
type _ carrier =
  | ParticipantC : participant carrier
  | AssistantC : assistant carrier
  | ExperimenterC : experimenter carrier
  | LocationManagerC : location_manager carrier
  | RecruiterC : recruiter carrier
  | OperatorC : operator carrier

let equal : type person. person t -> person t -> bool =
 fun p1 p2 ->
  match p1, p2 with
  | Participant one, Participant two
  | Assistant one, Assistant two
  | Experimenter one, Experimenter two
  | LocationManager one, LocationManager two
  | Recruiter one, Recruiter two
  | Operator one, Operator two -> equal_person one two
;;

let pp : type person. Format.formatter -> person t -> unit =
 fun formatter person ->
  match person with
  | Participant m
  | Assistant m
  | Experimenter m
  | LocationManager m
  | Recruiter m
  | Operator m -> pp_person formatter m
;;

type any = Any : 'a t -> any

let equal_any one two =
  let id model =
    match model with
    | Any (Participant { user; _ })
    | Any (Assistant { user; _ })
    | Any (Experimenter { user; _ })
    | Any (LocationManager { user; _ })
    | Any (Recruiter { user; _ })
    | Any (Operator { user; _ }) -> user.Sihl_user.id
  in
  String.equal (id one) (id two)
;;

let pp_any f (Any m) = pp f m

module Duplicate = struct
  type t =
    { first : any [@equal equal_any] [@printer pp_any]
    ; second : any [@equal equal_any] [@printer pp_any]
    ; ignored_at : Ptime.t option
    }
  [@@deriving eq, show]
end
