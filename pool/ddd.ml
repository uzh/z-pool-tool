(* Framework Infrastructure *)

let todo = failwith "todo"

module User = struct
  type t
end

type timestamp = unit
type 'a io = 'a

(* Application Infrastructure *)

type language =
  | En
  | De

type setting =
  | Languages of language list
  | Email_suffixes of string list

type event =
  | SignedUp of User.t
  | SettingsChanged of setting list

type command_result = (event list, string) Result.t

(* https://gitlab.uzh.ch/econ/study-coordination/pool/-/issues/5 *)

module ChangeSettings : sig
  type t

  val handle : t -> command_result
end = struct
  type t = setting list

  let[@warning "-27"] handle command = todo
end

(* Domain *)

(* https://gitlab.uzh.ch/econ/study-coordination/pool/-/issues/9 *)

type recruitment_channel =
  | Friend
  | Online
  | Lecture
  | Mailing

module SignUp : sig
  type t

  val handle
    :  t
    -> ?allowed_email_suffixes:string list
    -> ?password_policy:(string -> (unit, string) Result.t)
    -> command_result
end = struct
  type t =
    { email : string
    ; password : string
    ; firstname : string
    ; lastname : string
    ; recruitment_channel : recruitment_channel
    ; terms_accepted_at : timestamp
    }

  let handle _ ?allowed_email_suffixes:_ ?password_policy:_ = todo
end

(* https://gitlab.uzh.ch/econ/study-coordination/pool/-/issues/3 *)

type client =
  { id : string
  ; title : string
  ; description : string
  ; url : string
  ; database : string
  ; styles : string
  ; icon : string
  ; logos : string
  ; partner_logos : string
  ; disabled : bool
  }

module AddClient : sig
  type t

  val handle : t -> command_result
end = struct
  type t

  let handle = todo
end

module EditClient : sig
  type t

  val handle : t -> client -> command_result
end = struct
  type t

  let handle _ _ = todo
end

type operator = unit

module AddOperator : sig
  type t

  val handle : t -> client -> command_result
end = struct
  type t

  let handle _ _ = todo
end

module ActivateOperator : sig
  type t

  val handle : t -> operator -> command_result
end = struct
  type t

  let handle _ _ = todo
end

module DeactivateOperator : sig
  type t

  val handle : t -> operator -> command_result
end = struct
  type t

  let handle _ _ = todo
end
