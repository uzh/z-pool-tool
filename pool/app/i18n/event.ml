open Entity

type create =
  { key : Key.t
  ; language : Pool_common.Language.t
  ; content : Content.t
  }
[@@deriving eq, show]

type event = Created of create

let handle_event pool : event -> unit Lwt.t = function
  | Created create ->
    let%lwt _ =
      Property.create create.key create.language create.content
      |> Repo.insert pool
    in
    Lwt.return_unit
;;

let[@warning "-4"] equal_event event1 event2 =
  match event1, event2 with
  | Created one, Created two -> equal_create one two
;;

let pp_event formatter event =
  match event with
  | Created m -> pp_create formatter m
;;
