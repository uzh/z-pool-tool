include Entity
include Event
include Default
module Guard = Entity_guard

let find = Repo.find
let find_with_default_content = Repo.find_with_default_content
let find_by_key = Repo.find_by_key
let find_by_key_opt = Repo.find_by_key_opt
let find_all = Repo.find_all

module PrivacyPolicyCache = struct
  open Hashtbl

  let tbl : (Pool_tenant.Database.Label.t * Pool_common.Language.t, bool) t =
    create 5
  ;;

  let find = find_opt tbl

  let add database_label language value =
    replace tbl (database_label, language) value
  ;;

  let clear () = clear tbl
end

let privacy_policy_is_set database_label language =
  let open Utils.Lwt_result.Infix in
  PrivacyPolicyCache.find (database_label, language)
  |> function
  | Some bool -> Lwt.return bool
  | None ->
    let%lwt existing =
      find_by_key_opt database_label Entity.Key.PrivacyPolicy language
      ||> CCOption.is_some
    in
    let () = PrivacyPolicyCache.add database_label language existing in
    Lwt.return existing
;;
