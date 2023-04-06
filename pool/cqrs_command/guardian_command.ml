let src = Logs.Src.create "guardian.cqrs"

module CreateRule : sig
  include Common.CommandSig with type t = Guard.Rule.t

  val handle
    :  ?tags:Logs.Tag.set
    -> Guard.Rule.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val validate
    :  'a Guard.Actor.t
    -> Guard.Rule.t
    -> (unit, Pool_common.Message.error) result
end = struct
  type t = Guard.Rule.t

  let handle ?(tags = Logs.Tag.empty) rule =
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
    Ok [ Guard.RulesSaved [ rule ] |> Pool_event.guard ]
  ;;

  let validate actor ((role, _, _) : Guard.Rule.t) =
    let open Guard in
    (match role with
     | ActorSpec.Entity role -> actor |> Actor.roles |> RoleSet.mem role
     | ActorSpec.Id (role, uuid) ->
       actor |> Actor.id |> Uuid.Actor.equal uuid
       && actor |> Actor.roles |> RoleSet.mem role)
    |> function
    | true -> Ok ()
    | false -> Error Pool_common.Message.PermissionDeniedCreateRule
  ;;

  let effects = Guard.Access.manage_rules
end
