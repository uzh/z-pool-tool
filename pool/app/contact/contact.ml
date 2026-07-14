module Guard = Entity_guard
module VersionHistory = Version_history
include Repo
include Entity
include Event

let list_by_user = Repo.list_by_user

let find_by_user pool (user : Pool_user.t) =
  user.Pool_user.id |> Id.of_user |> Repo.find pool
;;

let has_terms_accepted pool (contact : t) =
  let%lwt last_updated = I18n.terms_and_conditions_last_updated pool in
  let terms_accepted_at =
    contact.terms_accepted_at |> CCOption.map Pool_user.TermsAccepted.value
  in
  CCOption.map_or ~default:false (Ptime.is_later ~than:last_updated) terms_accepted_at
  |> Lwt.return
;;

let deduplicate contacts =
  let seen : (string, unit) Hashtbl.t = Hashtbl.create (CCList.length contacts) in
  contacts
  |> CCList.filter (fun { user = { Pool_user.id; _ }; _ } ->
    let id = Id.of_user id in
    match Hashtbl.find_opt seen id with
    | Some () -> false
    | None ->
      Hashtbl.add seen id ();
      true)
;;

module Repo = struct
  include Repo_entity

  let joins = Repo.joins
  let joins_with_promoted = Repo.joins_with_promoted
  let sql_select_columns = Repo.sql_select_columns
  let sql_select_columns_with_promoted = Repo.sql_select_columns_with_promoted
  let make_sql_select_columns = Repo.make_sql_select_columns
  let find_request_sql = Repo.find_request_sql
  let update_request = Repo.update_request
  let invitable_sql_condition = Repo.invitable_sql_condition
end
