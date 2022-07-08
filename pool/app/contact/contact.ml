include Entity
include Event

let find = Repo.find
let find_multiple = Repo.find_multiple
let find_filtered = Repo.find_filtered
let find_by_email = Repo.find_by_email
let find_all = Repo.find_all

let find_by_user pool (user : Sihl_user.t) =
  user.Sihl_user.id |> Pool_common.Id.of_string |> Repo.find pool
;;

let has_terms_accepted = Event.has_terms_accepted

module Repo = struct
  module Preview = Repo_model.Preview
end
