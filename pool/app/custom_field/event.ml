open Entity

type event =
  | AnswerUpserted of Public.t * Pool_common.Id.t
  | Created of t
  | OptionCreated of (Id.t * SelectOption.t)
  | OptionDestroyed of SelectOption.t
  | OptionUpdated of SelectOption.t
  | Updated of t
[@@deriving eq, show, variants]

let handle_event pool : event -> unit Lwt.t = function
  | AnswerUpserted (t, entity_uuid) ->
    Repo_public.upsert_answer pool entity_uuid t
  | Created t -> Repo.insert pool t
  | OptionCreated (field_id, t) -> Repo_option.insert pool field_id t
  | OptionDestroyed t -> Repo_option.destroy pool t
  | OptionUpdated t -> Repo_option.update pool t
  | Updated t -> Repo.update pool t
;;
