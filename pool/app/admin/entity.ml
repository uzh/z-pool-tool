module Common = Pool_common

module Stringify = struct
  let person = function
    | `Assistant -> "assistant"
    | `Experimenter -> "experimenter"
    | `LocationManager -> "location_manager"
    | `Recruiter -> "recruiter"
    | `Operator -> "operator"
  ;;

  let person_from_string = function
    | "assistant" -> Ok `Assistant
    | "experimenter" -> Ok `Experimenter
    | "location_manager" -> Ok `LocationManager
    | "recruiter" -> Ok `Recruiter
    | "operator" -> Ok `Operator
    | _ -> Error Pool_common.Message.(Invalid Role)
  ;;
end

type person =
  { user : Sihl_user.t
        [@equal fun m k -> CCString.equal m.Sihl_user.id k.Sihl_user.id]
  ; created_at : Common.CreatedAt.t
  ; updated_at : Common.UpdatedAt.t
  }
[@@deriving eq, show]

let create_person user =
  { user
  ; created_at = Common.CreatedAt.create ()
  ; updated_at = Common.UpdatedAt.create ()
  }
;;

type assistant
type experimenter
type location_manager
type recruiter
type operator

type _ t =
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

let equal : type person. person t -> person t -> bool =
 fun p1 p2 ->
  match p1, p2 with
  | Assistant one, Assistant two
  | Experimenter one, Experimenter two
  | LocationManager one, LocationManager two
  | Recruiter one, Recruiter two
  | Operator one, Operator two -> equal_person one two
;;

let pp : type person. Format.formatter -> person t -> unit =
 fun formatter person ->
  match person with
  | Assistant m | Experimenter m | LocationManager m | Recruiter m | Operator m
    -> pp_person formatter m
;;

type any_person = Any : 'a t -> any_person

let equal_any_person one two =
  let id model =
    match model with
    | Any (Assistant { user; _ })
    | Any (Experimenter { user; _ })
    | Any (LocationManager { user; _ })
    | Any (Recruiter { user; _ })
    | Any (Operator { user; _ }) -> user.Sihl_user.id
  in
  CCString.equal (id one) (id two)
;;

let pp_any_person f (Any m) = pp f m

let person : type p. p t -> person = function
  | Assistant p | Experimenter p | LocationManager p | Recruiter p | Operator p
    -> p
;;

let user : type p. p t -> Sihl_user.t = function
  | Assistant { user; _ }
  | Experimenter { user; _ }
  | LocationManager { user; _ }
  | Recruiter { user; _ }
  | Operator { user; _ } -> user
;;

module Duplicate = struct
  type t =
    { first : any_person [@equal equal_any_person] [@printer pp_any_person]
    ; second : any_person [@equal equal_any_person] [@printer pp_any_person]
    ; ignored_at : Ptime.t option
    }
  [@@deriving eq, show]
end
