include Entity
include Event

let find_all = Repo.find_all
let find = Repo.find
let find_all_for_contact = Repo_public.find_all_for_contact
let find_by_contact = Repo_public.find_by_contact
let upsert_answer = Repo_public.upsert_answer
