open Entity

type create =
  { experiment : Experiment.Public.t
  ; contact : Contact.t
  }
[@@deriving eq, show]

type update = { admin_comment : AdminComment.t option } [@@deriving eq, show]

type event =
  | Created of create
  | Updated of update * t
  | Deleted of Entity.t
[@@deriving eq, show]

let handle_event ?user_uuid pool : event -> unit Lwt.t =
  let open Utils.Lwt_result.Infix in
  let create_changelog before after =
    let open Version_history in
    insert pool ?user_uuid ~entity_uuid:before.id ~before ~after ()
  in
  function
  | Created { experiment; contact } ->
    let write =
      Repo_entity.Write.create
        (Contact.id contact)
        (experiment |> Experiment.Public.id)
        None
    in
    let%lwt () = Repo.insert pool write in
    write
    |> Entity_guard.Target.to_authorizable_of_write ~ctx:(Database.to_ctx pool)
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : Guard.Target.t) -> ()
  | Updated (command, waiting_list) ->
    let updated = { waiting_list with admin_comment = command.admin_comment } in
    let%lwt () = Repo.update pool updated in
    create_changelog waiting_list updated
  | Deleted m -> Repo.delete pool m
;;
