include Entity
include Event

let find_languages = Repo.find_languages
let find_email_suffixes = Repo.find_email_suffixes
let find_contact_email = Repo.find_contact_email
let find_inactive_user_disable_after = Repo.find_inactive_user_disable_after
let find_inactive_user_warning = Repo.find_inactive_user_warning
let find_terms_and_conditions = Repo.find_terms_and_conditions

let terms_and_conditions_last_updated pool =
  let open Lwt_result.Infix in
  find_terms_and_conditions pool () >|= updated_at
;;
