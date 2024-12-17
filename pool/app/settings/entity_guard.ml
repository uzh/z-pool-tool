module Access = struct
  open Guard
  open ValidationSet
  open Permission

  let setting action = one_of_tuple (action, `SystemSetting, None)
  let index = setting Read
  let create = setting Create
  let read = setting Read
  let update = setting Update
  let delete = setting Delete
end
