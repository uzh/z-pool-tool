include Entity
include Event

let run database_label contact_uuid =
  let open Utils.Lwt_result.Infix in
  let%lwt fields = Custom_field.find_for_duplicate_check database_label in
  Repo.find_similars database_label ~user_uuid:contact_uuid fields
  >|> Repo.insert database_label
;;

let find = Repo.find
let all = Repo.all
let find_by_contact = Repo.find_by_contact
