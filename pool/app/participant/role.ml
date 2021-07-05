let update (participant : Entity.participant) =
  "update_participant", Some participant.user.id
;;

let can_update (user : Sihl.User.t) (participant : Entity.participant)
    : bool Lwt.t
  =
  if not (String.equal participant.user.id user.id)
  then Authz.can user (update participant)
  else Lwt.return false
;;
