module Access = struct
  open Guard
  open ValidationSet

  let read = one_of_tuple (Permission.Read, `Statistics, None)
end
