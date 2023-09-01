module Access = struct
  open Guard
  open ValidationSet
  open Permission

  let index = one_of_tuple (Read, `Schedule, None)
end
