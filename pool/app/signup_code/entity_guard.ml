module Access = struct
  open Guard
  open ValidationSet
  open Permission

  let signup_code action uuid =
    one_of_tuple
      (action, `SignupCode, Some (uuid |> Uuid.target_of Entity.Id.value))
  ;;

  let index = one_of_tuple (Read, `SignupCode, None)
  let create = one_of_tuple (Create, `SignupCode, None)
  let update = signup_code Update
  let delete = signup_code Delete
end
