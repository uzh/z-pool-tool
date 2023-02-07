include Entity
include Event
module Guard = Entity_guard

module Repo = struct
  module Public = struct
    include Repo_public

    module Entity = struct
      include Repo_entity.Public
    end
  end

  include Repo
end

let find = Repo.find
let find_all = Repo.find_all
let find_public = Repo_public.find
let find_all_public_by_contact = Repo_public.find_all_public_by_contact

let find_where_contact_is_on_waitinglist =
  Repo_public.find_where_contact_is_on_waitinglist
;;

let find_of_session = Repo.find_of_session
let find_of_mailing = Repo.find_of_mailing
let session_count = Repo.session_count
let search = Repo.search
let multiple_search_results_by_id = Repo.multiple_search_results_by_id
let possible_participant_count _ = Lwt.return 0
let possible_participants _ = Lwt.return []
