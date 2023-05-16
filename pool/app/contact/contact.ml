include Entity
include Event
module Guard = Entity_guard

let find = Repo.find
let find_multiple = Repo.find_multiple
let find_by_email = Repo.find_by_email
let find_all = Repo.find_all
let find_to_trigger_profile_update = Repo.find_to_trigger_profile_update

let should_send_registration_attempt_notification =
  Repo.should_send_registration_attempt_notification
;;

let find_by_user pool (user : Sihl_user.t) =
  user.Sihl_user.id |> Pool_common.Id.of_string |> Repo.find pool
;;

let find_phone_number_verification_by_contact =
  Repo.find_phone_number_verification_by_contact
;;

let find_phone_number_verification_by_contact_and_token =
  Repo.find_phone_number_verification_by_contact_and_token
;;

let has_terms_accepted = Event.has_terms_accepted

let message_language database_label ({ language; _ } : t) =
  language
  |> CCOption.map_or
       ~default:(Settings.default_language database_label)
       Lwt.return
;;

module Repo = struct
  module Preview = Repo_model.Preview

  module Model = struct
    let t = Repo_model.t
  end

  module Sql = struct
    let find_request_sql = Repo_sql.find_request_sql
  end
end
