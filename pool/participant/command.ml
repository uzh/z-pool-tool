module SignUp : sig
  type t

  val handle
    :  t
    -> ?allowed_email_suffixes:string list
    -> ?password_policy:(string -> (unit, string) Result.t)
    -> (Event.t list, string) Result.t
end = struct
  type t = Entity.participant

  let handle _ ?allowed_email_suffixes:_ ?password_policy:_ = Sihl.todo
end
