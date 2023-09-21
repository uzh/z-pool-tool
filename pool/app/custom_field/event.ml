open Entity

type event =
  | AdminAnswerCleared of Public.t * Pool_common.Id.t
  | AnswerUpserted of Public.t * Pool_common.Id.t * Pool_context.user
  | AnsweredOnSignup of Public.t * Pool_common.Id.t
  | Created of t
  | Deleted of t
  | FieldsSorted of t list
  | GroupCreated of Group.t
  | GroupDestroyed of Group.t
  | GroupsSorted of Group.t list
  | GroupUpdated of Group.t
  | OptionCreated of (Id.t * SelectOption.t)
  | OptionDestroyed of SelectOption.t
  | OptionPublished of SelectOption.t
  | OptionsSorted of SelectOption.t list
  | OptionUpdated of SelectOption.t
  | PartialUpdate of PartialUpdate.t * Contact.t * Pool_context.user
  | Published of t
  | Updated of t
[@@deriving eq, show, variants]

let handle_event pool : event -> unit Lwt.t =
  let open Utils.Lwt_result.Infix in
  function
  | AdminAnswerCleared (m, entity_uuid) ->
    Repo_partial_update.clear_answer
      pool
      ~is_admin:true
      ~field_id:(Public.id m)
      ~entity_uuid
      ()
  | AnswerUpserted (m, entity_uuid, user) ->
    let is_admin = Pool_context.user_is_admin user in
    Repo_partial_update.upsert_answer pool is_admin entity_uuid m
  | AnsweredOnSignup (m, entity_uuid) ->
    Repo_partial_update.upsert_answer pool false entity_uuid m
  | Created m ->
    let%lwt () = Repo.insert pool m in
    Entity_guard.Target.to_authorizable ~ctx:(Pool_database.to_ctx pool) m
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : Guard.Target.t) -> ()
  | Deleted m -> Repo.delete pool m
  | FieldsSorted m -> CCList.map (fun m -> id m) m |> Repo.sort_fields pool
  | GroupCreated m ->
    let%lwt () = Repo_group.insert pool m in
    Entity_guard.Group.Target.to_authorizable ~ctx:(Pool_database.to_ctx pool) m
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : Guard.Target.t) -> ()
  | GroupDestroyed m -> Repo_group.destroy pool m
  | GroupsSorted m ->
    CCList.map (fun m -> m.Group.id) m |> Repo_group.sort_groups pool
  | GroupUpdated m -> Repo_group.update pool m
  | OptionCreated (field_id, m) -> Repo_option.insert pool field_id m
  | OptionDestroyed m -> Repo_option.destroy pool m
  | OptionPublished m -> Repo_option.publish pool m
  | OptionsSorted m ->
    CCList.map (fun m -> m.SelectOption.id) m |> Repo_option.sort_options pool
  | OptionUpdated m -> Repo_option.update pool m
  | PartialUpdate (update, contact, user) ->
    Repo_partial_update.update pool user update contact
  | Published m -> Repo.publish pool m
  | Updated m -> Repo.update pool m
;;
