include Entity
include Event

let find = Repo.find

module Human = struct
  include Entity_human
end

module Utils = struct
  include Filter_utils
end

module Repo = struct
  let t = Repo_entity.t
end

let all_keys tenant_db =
  let open Lwt.Infix in
  let open Custom_field in
  find_by_model tenant_db Model.Contact
  >|= CCList.map Key.customfield
  >|= CCList.append Key.(all_hardcoded |> CCList.map hardcoded)
;;

let key_of_string tenant_db str =
  let open Lwt_result.Infix in
  let open Key in
  match Key.read str with
  | Some hardcoded -> (Hardcoded hardcoded : human) |> Lwt_result.return
  | None ->
    str
    |> Custom_field.Id.of_string
    |> Custom_field.find tenant_db
    >|= fun field : human -> CustomField field
;;
