open Entity

type event =
  | Created of t
  | Updated of t
  | SignedUp of Code.t
  | Verified of Code.t
  | WebhookTriggered of Webhook.t
  | WebhookResent of Webhook.t * Pool_queue.Id.t
[@@deriving eq, show, variants]

let handle_event pool : event -> unit Lwt.t = function
  | Created signup_code -> Repo.insert pool signup_code
  | Updated signup_code -> Repo.update pool signup_code
  | SignedUp code -> Repo.increment pool `Signup code
  | Verified code -> Repo.increment pool `Verification code
  | WebhookTriggered webhook -> Webhook_job.dispatch pool webhook
  | WebhookResent (webhook, job_id) ->
    Webhook_job.dispatch ~job_ctx:(Pool_queue.job_ctx_clone job_id) pool webhook
;;

let with_webhook counted ?signup_code ?signup_webhook () =
  CCOption.(to_list (map counted signup_code))
  @ CCOption.(to_list (map webhooktriggered signup_webhook))
;;

let signed_up_events = with_webhook signedup
let verified_events = with_webhook verified
