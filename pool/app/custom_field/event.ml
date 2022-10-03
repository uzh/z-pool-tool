open Entity

type event =
  | AnswerUpserted of Public.t * Pool_common.Id.t
  | Created of t
  | Updated of t
[@@deriving eq, show, variants]

let handle_event pool : event -> unit Lwt.t = function
  | AnswerUpserted (t, entity_uuid) ->
    Repo_public.upsert_answer pool entity_uuid t
  | Created t -> Repo.insert pool t
  | Updated t -> Repo.update pool t
;;
