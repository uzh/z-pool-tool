let start ~context:_ =
  match Sys.getenv_opt "MEMTRACE" with
  | None | Some "" -> ()
  | Some _ ->
    prerr_endline "memtrace disabled: not installed, see opam install --with-dev-setup"
;;
