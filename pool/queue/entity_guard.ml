module Access = struct
  open Guard
  open ValidationSet
  open Permission
  open TargetEntity

  let index = One (Read, Model `Queue)
  let read = One (Read, Model `Queue)
end
