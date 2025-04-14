include Entity
include Event

let find_exn = Repo.find_exn
let find_opt = Repo.find_opt

let text_messages_enabled pool =
  let open Utils.Lwt_result.Infix in
  find_opt pool ||> CCOption.is_some
;;

let clear_cache = Repo.Cache.clear
