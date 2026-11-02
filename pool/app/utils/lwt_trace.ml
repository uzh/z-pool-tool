(* This module exists to add useful monadic infix operators and to ensure all monadic
   actions are implemented using the lwt ppx to always present clean stack/back traces
   (https://ocsigen.org/lwt/latest/manual/manual and
   https://github.com/ocsigen/lwt/blob/6ce3d557798d2b5736fb458b697cc3eaa13c461e/src/core/lwt.ml#L1694)

   The binds return the promise of [f] directly (tail position) instead of awaiting it
   with another [let%lwt]: otherwise a recursive loop like [Lwt_unix.sleep d >|> loop]
   keeps one pending promise per iteration alive. The backtrace stays complete, as [f]
   records its own frames; only a re-raise frame pointing at this module is dropped. *)

module Infix = struct
  let ( let* ) l f =
    let%lwt l' = l in
    match l' with
    | Error _ as e' -> Lwt.return e'
    | Ok x -> f x
  ;;

  (* Lwt *)
  (* Lwt.bind *)
  let ( >|> ) l f =
    let%lwt l' = l in
    f l'
  ;;

  (* Lwt.map *)
  let ( ||> ) l f =
    let%lwt l' = l in
    Lwt.return @@ f l'
  ;;

  (* Utils.Lwt_result *)
  (* Utils.Lwt_result.bind *)
  let ( >>= ) = ( let* )

  (* Utils.Lwt_result.bind_result *)
  let ( >== ) l f =
    let* l' = l in
    Lwt.return @@ f l'
  ;;

  (* Utils.Lwt_result.bind_lwt *)
  let ( |>> ) l f =
    let* l' = l in
    let%lwt l'' = f l' in
    Lwt_result.return l''
  ;;

  (* Utils.Lwt_result.map *)
  let ( >|+ ) l f =
    let* l' = l in
    Lwt_result.return @@ f l'
  ;;

  (* Utils.Lwt_result.map_error *)
  let ( >|- ) l f =
    let%lwt l' = l in
    match l' with
    | Error e -> Lwt.return (Error (f e))
    | Ok _ as x' -> Lwt.return x'
  ;;
end

module ParallelInfix = struct
  open Infix

  (* Utils.Lwt_result.bind CCFun.const *)
  let ( >> ) m k = m >>= fun _ -> k
end

let map_error f l = Infix.( >|- ) l f
