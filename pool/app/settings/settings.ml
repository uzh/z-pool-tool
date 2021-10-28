include Entity
include Event

let allowed_email_suffixes = Utils.todo

let terms_and_conditions =
  Logs.warn (fun m -> m "TODO terms and conditions");
  let terms_and_conditions =
    TermsAndConditions.create "Please accept all terms and condition."
  in
  Setting.
    { setting = SettingValue.TermsAndConditions terms_and_conditions
    ; created_at = Ptime_clock.now ()
    ; updated_at = Ptime_clock.now ()
    }
  |> Lwt.return
;;

let last_updated (setting : Setting.t) : Ptime.t = setting.Setting.updated_at
