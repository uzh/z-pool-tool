include Entity
include Event
module Guard = Entity_guard

let find = Repo.find
let find_with_default_content = Repo.find_with_default_content
let find_by_key_opt = Repo.find_by_key_opt
let find_all = Repo.find_all
let terms_and_conditions_last_updated = Repo.terms_and_conditions_last_updated

module I18nCache = struct
  open Hashtbl

  let privacy_policy : (Database.Label.t * Pool_common.Language.t, bool) t = create 5

  let tbl : (Database.Label.t * Entity.Key.t * Pool_common.Language.t, Entity.t) t =
    create 5
  ;;

  let find = find_opt tbl

  let add database_label key language value =
    replace tbl (database_label, key, language) value
  ;;

  let clear () =
    let () = clear tbl in
    clear privacy_policy
  ;;
end

let privacy_policy_is_set database_label language =
  let open Utils.Lwt_result.Infix in
  Hashtbl.find_opt I18nCache.privacy_policy (database_label, language)
  |> function
  | Some bool -> Lwt.return bool
  | None ->
    let%lwt existing =
      find_by_key_opt database_label Key.PrivacyPolicy language ||> CCOption.is_some
    in
    let () = Hashtbl.add I18nCache.privacy_policy (database_label, language) existing in
    Lwt.return existing
;;

let find_by_key database_label key language =
  I18nCache.find (database_label, key, language)
  |> function
  | Some i18n -> Lwt.return i18n
  | None ->
    let%lwt i18n = Repo.find_by_key database_label key language in
    let () = I18nCache.add database_label key language i18n in
    Lwt.return i18n
;;
