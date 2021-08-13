module Email = struct
  type t = string [@@deriving eq, show]
end

module Password = struct
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
end

module Disabled = struct
  type t = bool [@@deriving eq, show]
end

module Verified = struct
  type t = Ptime.t [@@deriving eq, show]
end

module RecruitmentChannel = struct
  type t =
    | Friend
    | Online
    | Lecture
    | Mailing
  [@@deriving eq, show]
end

type person =
  { user : Sihl.User.t
  ; recruitment_channel : RecruitmentChannel.t
  ; terms_accepted_at : Sihl.timestamp
  ; paused : Paused.t
  ; disabled : Disabled.t
  ; verified : Verified.t
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

type duplicate =
  { first : (participant t, experimenter t) Utils.OneOf2.t
  ; second : (participant t, experimenter t) Utils.OneOf2.t
  ; ignored_at : Ptime.t option
  }
