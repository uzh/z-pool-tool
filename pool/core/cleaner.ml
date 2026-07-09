(* Extracted from the Sihl web framework (https://github.com/oxidizing/sihl), MIT license.
   Source:
   https://github.com/uzh/sihl/blob/6c3c4040413294155cda3a363edf0ff3c7e638b8/sihl/src/core_cleaner.ml
   The unused [register_cleaners] function has been removed. *)

let registered_cleaners : (?ctx:(string * string) list -> unit -> unit Lwt.t) list ref =
  ref []
;;

let register_cleaner cleaner =
  registered_cleaners := List.cons cleaner !registered_cleaners
;;

let clean_all ?ctx () =
  let cleaners = !registered_cleaners in
  let rec clean_repos ?ctx cleaners =
    match cleaners with
    | [] -> Lwt.return ()
    | cleaner :: cleaners ->
      let%lwt () = cleaner ?ctx () in
      clean_repos cleaners
  in
  clean_repos ?ctx cleaners
;;
