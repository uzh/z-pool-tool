module Access = struct
  open Guard
  open ValidationSet
  open Permission

  let index ?id () = one_of_tuple (Read, `Queue, id)
  let read = one_of_tuple (Read, `Queue, None)
  let resend = one_of_tuple (Create, `Queue, None)
end
