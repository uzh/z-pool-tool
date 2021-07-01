(* Infrastructure *)

let todo = failwith "todo"

module User = struct
  type t
end

type timestamp = unit
type data = (string * string list) list
type 'a io = 'a

(* Domain *)

type event = SignedUp of User.t
type command_result = (event list, string) Result.t

(* https://gitlab.uzh.ch/econ/study-coordination/pool/-/issues/9 *)

type recruitment_channel =
  | Friends
  | Ads
  | Search

module SignUp : sig
  type t

  val handle
    :  t
    -> ?allowed_email_suffixes:string list
    -> ?password_policy:(string -> (unit, string) Result.t)
    -> User.t list
    -> command_result

  val of_data : data -> command_result io
end = struct
  type t =
    { email : string
    ; password : string
    ; firstname : string
    ; lastname : string
    ; recruitment_channel : recruitment_channel
    ; terms_accepted_at : timestamp
    }

  let[@warning "-27"] handle
      command
      ?(allowed_email_suffixes = [])
      ?(password_policy = todo)
      similar_users
    =
    todo
  ;;

  let[@warning "-27"] of_data data = todo
end
