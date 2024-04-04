open Entity

type create =
  { key : Key.t
  ; language : Pool_common.Language.t
  ; content : Content.t
  }
[@@deriving eq, show]

type event =
  | Created of create
  | Updated of t * Content.t
[@@deriving eq, show]

let insert_i18n pool i18n =
  let open Utils.Lwt_result.Infix in
  let%lwt () = Repo.insert pool i18n in
  i18n
  |> Entity_guard.Target.to_authorizable ~ctx:(Database.to_ctx pool)
  ||> Pool_common.Utils.get_or_failwith
  ||> fun (_ : Guard.Target.t) -> ()
;;

let handle_event pool : event -> unit Lwt.t = function
  | Created create ->
    let i18n = Entity.create create.key create.language create.content in
    insert_i18n pool i18n
  | Updated (m, content) -> { m with content } |> Repo.update pool
;;
