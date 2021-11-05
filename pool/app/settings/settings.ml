include Entity
include Event

let find_languages = Repo.find_languages
let find_email_suffixes = Repo.find_email_suffixes
let find_contact_email = Repo.find_contact_email
let find_inactive_user_disable_after = Repo.find_inactive_user_disable_after
let find_inactive_user_warning = Repo.find_inactive_user_warning

let terms_and_conditions =
  "Please accept all terms and condition." |> Lwt.return
;;

let last_updated (setting : t) : Ptime.t = setting.updated_at
