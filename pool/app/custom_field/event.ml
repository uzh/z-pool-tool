open Entity

type event =
  | AnswerUpserted of Public.t * Pool_common.Id.t
  | Created of t
  | GroupCreated of Group.t
  | GroupDestroyed of Group.t
  | GroupsSorted of Group.t list
  | GroupUpdated of Group.t
  | OptionCreated of (Id.t * SelectOption.t)
  | OptionDestroyed of SelectOption.t
  | OptionsSorted of SelectOption.t list
  | OptionUpdated of SelectOption.t
  | Updated of t
[@@deriving eq, show, variants]

let handle_event pool : event -> unit Lwt.t = function
  | AnswerUpserted (t, entity_uuid) ->
    Repo_public.upsert_answer pool entity_uuid t
  | Created t -> Repo.insert pool t
  | GroupCreated t -> Repo_group.insert pool t
  | GroupDestroyed t -> Repo_group.destroy pool t
  | GroupsSorted t ->
    CCList.map (fun o -> o.Group.id) t |> Repo_group.sort_groups pool
  | GroupUpdated t -> Repo_group.update pool t
  | OptionCreated (field_id, t) -> Repo_option.insert pool field_id t
  | OptionDestroyed t -> Repo_option.destroy pool t
  | OptionUpdated t -> Repo_option.update pool t
  | OptionsSorted t ->
    CCList.map (fun o -> o.SelectOption.id) t |> Repo_option.sort_options pool
  | Updated t -> Repo.update pool t
;;
