include Entity
include Event

type add = t -> t Lwt.t
type update = t -> t Lwt.t
type destroy = t -> t Lwt.t

let find = Repo.find
let find_all = Repo.find_all
let possible_participant_count _ = Lwt.return 0
let possible_participants _ = Lwt.return []
