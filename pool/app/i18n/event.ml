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
  | Edited of Property.t * edit

let handle_event pool : event -> unit Lwt.t = function
  | Created create ->
    let%lwt _ =
      Property.create create.key create.language create.content
      |> Repo.insert pool
    in
    Lwt.return_unit
  | Edited (property, update) ->
    let%lwt _ =
      Property.{ property with content = update.content } |> Repo.update pool
    in
    Lwt.return_unit
;;

let[@warning "-4"] equal_event event1 event2 =
  match event1, event2 with
  | Created one, Created two -> equal_create one two
  | Edited (property_one, edit_one), Edited (property_two, edit_two) ->
    Property.equal property_one property_two && equal_edit edit_one edit_two
  | _ -> false
;;

let pp_event formatter event =
  match event with
  | Created m -> pp_create formatter m
  | Edited (property, m) ->
    let () = Property.pp formatter property in
    pp_edit formatter m
;;
