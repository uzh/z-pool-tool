module Access = struct
  open Guard
  open ValidationSet

  let index = One (Action.Read, TargetSpec.Entity `Schedule)
end
