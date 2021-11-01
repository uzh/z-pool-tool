include Entity
include Event

let find_languages = Repo.find_languages
let find_email_suffixes = Repo.find_email_suffixes
let find_contact_email = Repo.find_contact_email

let terms_and_conditions =
  "Please accept all terms and condition." |> Lwt.return
;;

let last_updated (setting : t) : Ptime.t = setting.updated_at
