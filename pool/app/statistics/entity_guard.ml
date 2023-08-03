module Access = struct
  open Guard
  open ValidationSet

  let read = One (Action.Read, TargetSpec.Entity `Statistics)
end
