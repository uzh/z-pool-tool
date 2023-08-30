module Access = struct
  open Guard
  open ValidationSet

  let read = One (Permission.Read, TargetEntity.Model `Statistics)
end
