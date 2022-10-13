include Entity
include Event

let find_all = Repo.find_all
let find_by_model = Repo.find_by_model
let find_by_group = Repo.find_by_group
let find = Repo.find
let find_public = Repo_public.find

let find_all_by_contact ?is_admin pool id =
  Repo_public.find_all_by_contact ?is_admin pool id
;;

let find_all_required_by_contact pool id =
  Repo_public.find_all_required_by_contact pool id
;;

let find_multiple_by_contact = Repo_public.find_multiple_by_contact
let find_by_contact = Repo_public.find_by_contact
let upsert_answer = Repo_public.upsert_answer
let all_required_answered = Repo_public.all_required_answered
let find_option = Repo_option.find

let find_options_by_field pool id =
  let open Lwt.Infix in
  Repo_option.find_by_field pool id >|= CCList.map Repo_entity.Option.to_entity
;;

let find_group = Repo_group.find
let find_groups_by_model = Repo_group.find_by_model
