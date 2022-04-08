open Entity

type create =
  { key : Key.t
  ; language : Pool_common.Language.t
  ; content : Content.t
  }
[@@deriving eq, show]

type edit = { content : Content.t } [@@deriving eq, show]

type event =
  | Created of create
  | Updated of t * edit
  | DefaultRestored of t list

let handle_event pool : event -> unit Lwt.t = function
  | Created create ->
    let%lwt () =
      Entity.create create.key create.language create.content
      |> Repo.insert pool
    in
    Lwt.return_unit
  | Updated (property, update) ->
    let%lwt () =
      { property with content = update.content } |> Repo.update pool
    in
    Lwt.return_unit
  | DefaultRestored default_values ->
    let%lwt () = Lwt_list.iter_s (Repo.delete_by_key pool) Key.all in
    let%lwt () = Lwt_list.iter_s (Repo.insert pool) default_values in
    Lwt.return_unit
;;

let equal_event event1 event2 =
  match event1, event2 with
  | Created one, Created two -> equal_create one two
  | Updated (property_one, edit_one), Updated (property_two, edit_two) ->
    equal property_one property_two && equal_edit edit_one edit_two
  | DefaultRestored one, DefaultRestored two ->
    CCList.map2
      equal
      (one |> CCList.stable_sort compare)
      (two |> CCList.stable_sort compare)
    |> CCList.for_all CCFun.id
  | (Created _ | Updated _ | DefaultRestored _), _ -> false
;;

let pp_event formatter event =
  match event with
  | Created m -> pp_create formatter m
  | Updated (property, m) ->
    let () = pp formatter property in
    pp_edit formatter m
  | DefaultRestored m -> CCList.iter (pp formatter) m
;;
