include Entity
include Event
module Guard = Entity_guard

let find = Repo.find
let find_with_default_content = Repo.find_with_default_content
let find_by_key = Repo.find_by_key
let find_by_key_opt = Repo.find_by_key_opt
let find_all = Repo.find_all

module I18nPageCache = struct
  open Hashtbl

  let tbl
    : ( Pool_tenant.Database.Label.t * Entity.Key.t * Pool_common.Language.t
        , bool )
        t
    =
    create 5
  ;;

  let find = find_opt tbl

  let add database_label key language value =
    replace tbl (database_label, key, language) value
  ;;

  let clear () = clear tbl
end

let i18n_is_set database_label language key =
  let open Utils.Lwt_result.Infix in
  I18nPageCache.find (database_label, key, language)
  |> function
  | Some bool -> Lwt.return bool
  | None ->
    let%lwt existing =
      find_by_key_opt database_label key language ||> CCOption.is_some
    in
    let () = I18nPageCache.add database_label key language existing in
    Lwt.return existing
;;
