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
  module Entity = Repo_entity
end

let find = Repo.find
let find_all = Repo.find_all
let find_public = Repo_public.find
let find_full_by_contact = Repo_public.find_full_by_contact
let find_all_public_by_contact = Repo_public.find_all_public_by_contact

let find_pending_waitinglists_by_contact =
  Repo_public.find_pending_waitinglists_by_contact
;;

let find_past_experiments_by_contact =
  Repo_public.find_past_experiments_by_contact
;;

let find_of_session = Repo.find_of_session
let find_of_mailing = Repo.find_of_mailing
let session_count = Repo.session_count
let search = Repo.search
let search_multiple_by_id = Repo.search_multiple_by_id
let possible_participant_count _ = Lwt.return 0
let possible_participants _ = Lwt.return []

let smtp_auth database_label ({ smtp_auth_id; _ } : t) =
  let open Utils.Lwt_result.Infix in
  match smtp_auth_id with
  | None -> Lwt_result.return None
  | Some id -> Email.SmtpAuth.find database_label id >|+ CCOption.return
;;

let find_contact_person database_label { contact_person_id; _ } =
  let open Utils.Lwt_result.Infix in
  contact_person_id
  |> CCOption.map_or ~default:Lwt.return_none (fun id ->
       id |> Admin.find database_label ||> CCResult.to_opt)
;;
