include Entity
include Event
include Repo

let text_messages_enabled pool =
  let open Utils.Lwt_result.Infix in
  find_opt pool ||> CCOption.is_some
;;

let clear_cache = Repo.Cache.clear
