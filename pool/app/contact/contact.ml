include Entity
include Event
module Guard = Entity_guard

let find = Repo.find
let find_admin_comment = Repo.find_admin_comment
let find_multiple = Repo.find_multiple
let find_by_email = Repo.find_by_email
let find_all = Repo.find_all
let find_to_trigger_profile_update = Repo.find_to_trigger_profile_update

let should_send_registration_attempt_notification =
  Repo.should_send_registration_attempt_notification
;;

let find_by_user pool (user : Pool_user.t) = user.Pool_user.id |> Repo.find pool

let find_cell_phone_verification_by_contact =
  Repo.find_cell_phone_verification_by_contact
;;

let find_cell_phone_verification_by_contact_and_code =
  Repo.find_cell_phone_verification_by_contact_and_code
;;

let find_full_cell_phone_verification_by_contact =
  Repo.find_full_cell_phone_verification_by_contact
;;

let has_terms_accepted pool (contact : t) =
  let%lwt last_updated = I18n.terms_and_conditions_last_updated pool in
  let terms_accepted_at =
    contact.terms_accepted_at |> CCOption.map User.TermsAccepted.value
  in
  CCOption.map_or
    ~default:false
    (Ptime.is_later ~than:last_updated)
    terms_accepted_at
  |> Lwt.return
;;

module Repo = struct
  module Preview = Repo_model.Preview
  module Entity = Repo_model

  let joins = Repo_sql.joins
  let sql_select_columns = Repo_sql.sql_select_columns
  let find_request_sql = Repo_sql.find_request_sql
end
