include Entity
include Event

let find = Repo.find
let find_all = Repo.find_all

(* TODO[timhub]: implement session count, when sessions were implemented *)
let session_count _ _ = Lwt_result.return 0
let possible_participant_count _ = Lwt.return 0
let possible_participants _ = Lwt.return []
