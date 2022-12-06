open Entity
open Default

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
  | DefaultRestored of default
[@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t =
  let open Utils.Lwt_result.Infix in
  function
  | Created create ->
    let i18n = Entity.create create.key create.language create.content in
    let%lwt () = Repo.insert pool i18n in
    i18n
    |> Entity_guard.Target.to_authorizable ~ctx:(Pool_tenant.to_ctx pool)
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : [> `I18n ] Guard.AuthorizableTarget.t) -> ()
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
