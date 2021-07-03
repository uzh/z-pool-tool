module Permission = struct
  let update (participant : Entity.participant) =
    "update_participant", participant.user.id
  ;;
end

let can_update (user : Sihl.User.t) (participant : Entity.participant)
    : bool Lwt.t
  =
  if not (String.equal participant.user.id user.id)
  then Authz.can user (Permission.update participant)
  else Lwt.return false
;;
