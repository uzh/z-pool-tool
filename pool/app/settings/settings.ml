include Entity
include Event
module RepoEntity = Repo_entity

let find_languages = Repo.find_languages
let find_email_suffixes = Repo.find_email_suffixes
let allowed_email_suffixes = Utils.todo

let terms_and_conditions =
  Logs.warn (fun m -> m "TODO terms and conditions");
  let terms_and_conditions =
    TermsAndConditions.create "Please accept all terms and condition."
  in
  { setting = SettingValue.TermsAndConditions terms_and_conditions
  ; created_at = Ptime_clock.now ()
  ; updated_at = Ptime_clock.now ()
  }
  |> Lwt.return
;;

let last_updated (setting : t) : Ptime.t = setting.updated_at
