(* let send_reminder pool session = let open Lwt_result.Syntax in let*
   assignments = Assignment.find_uncanceled_by_session pool session.Session.id
   in assignments ;; *)
