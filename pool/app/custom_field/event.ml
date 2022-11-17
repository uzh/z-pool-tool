open Entity

type event =
  | AnswerUpserted of Public.t * Pool_common.Id.t
  | Created of t
  | Deleted of t
  | FieldsSorted of t list
  | GroupCreated of Group.t
  | GroupDestroyed of Group.t
  | GroupsSorted of Group.t list
  | GroupUpdated of Group.t
  | OptionCreated of (Id.t * SelectOption.t)
  | OptionDestroyed of SelectOption.t
  | OptionsSorted of SelectOption.t list
  | OptionUpdated of SelectOption.t
  | Published of t
  | Updated of t
[@@deriving eq, show, variants]

let handle_event pool : event -> unit Lwt.t = function
  | AnswerUpserted (m, entity_uuid) ->
    Repo_public.upsert_answer pool entity_uuid m
  | Created m -> Repo.insert pool m
  | Deleted m -> Repo.delete pool m
  | FieldsSorted m -> CCList.map (fun m -> id m) m |> Repo.sort_fields pool
  | GroupCreated m -> Repo_group.insert pool m
  | GroupDestroyed m -> Repo_group.destroy pool m
  | GroupsSorted m ->
    CCList.map (fun m -> m.Group.id) m |> Repo_group.sort_groups pool
  | GroupUpdated m -> Repo_group.update pool m
  | OptionCreated (field_id, m) -> Repo_option.insert pool field_id m
  | OptionDestroyed m -> Repo_option.destroy pool m
  | OptionUpdated m -> Repo_option.update pool m
  | OptionsSorted m ->
    CCList.map (fun m -> m.SelectOption.id) m |> Repo_option.sort_options pool
  | Published m -> Repo.publish pool m
  | Updated m -> Repo.update pool m
;;
