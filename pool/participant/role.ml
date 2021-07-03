module Permission = struct
  let update = "update_participant"
end

let can_update (user : Sihl.User.t) (participant : Entity.participant)
    : bool Lwt.t
  =
  if not (String.equal participant.user.id user.id)
  then Authz.can user Permission.update
  else Lwt.return false
;;
