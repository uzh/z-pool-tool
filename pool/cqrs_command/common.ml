module type CommandSig = sig
  type t

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Guard.ValidationSet.t
end

let guardian_cache_cleared_event ?id () =
  System_event.(
    Job.GuardianCacheCleared |> create ?id |> created |> Pool_event.system_event)
;;
