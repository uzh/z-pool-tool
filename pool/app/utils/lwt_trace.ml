(* This module exists to add useful monadic infix operators. We implement them using
   direct Lwt/Lwt_result primitives (bind/map) to avoid creating additional ppx
   binding frames that can appear as "re-raise" sites in backtraces. *)

module Infix = struct
  let ( let* ) = Lwt_result.bind

  (* Lwt *)
  (* Lwt.bind *)
  let ( >|> ) = Lwt.bind

  (* Lwt.map *)
  let ( ||> ) l f = Lwt.map f l

  (* Lwt_result *)
  (* Lwt_result.bind *)
  let ( >>= ) = ( let* )

  (* Lwt_result.bind_result *)
  let ( >== ) = Lwt_result.bind_result

  (* Lwt_result.bind CCFun.const *)
  let ( >> ) m k = m >>= fun _ -> k

  (* Lwt_result.bind_lwt *)
  let ( |>> ) = Lwt_result.bind_lwt

  (* Lwt_result.map *)
  let ( >|+ ) l f = Lwt_result.map f l

  (* Lwt_result.map_error *)
  let ( >|- ) l f = Lwt_result.map_error f l
end
