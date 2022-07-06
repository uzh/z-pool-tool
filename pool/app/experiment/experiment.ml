include Entity
include Event

let find = Repo.find
let find_all = Repo.find_all
let find_public = Repo_public.find
let find_all_public_by_contact = Repo_public.find_all_public_by_contact

(* TODO[timhub]: implement session count, when sessions were implemented *)
let session_count _ _ = Lwt_result.return 0
let possible_participant_count _ = Lwt.return 0
let possible_participants _ = Lwt.return []

module Repo = struct
  module Description = Repo_entity.Description
  module DirectRegistrationDisabled = Repo_entity.DirectRegistrationDisabled
end
