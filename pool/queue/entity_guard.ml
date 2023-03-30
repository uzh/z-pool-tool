module Access = struct
  open Guard
  open ValidationSet

  let index = One (Action.Read, TargetSpec.Entity `Queue)
  let read = One (Action.Read, TargetSpec.Entity `Queue)
end
