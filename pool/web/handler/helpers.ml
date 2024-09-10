module ContactUpdate = Helpers_contact_update
module Login = Helpers_login
module PartialUpdate = Helpers_partial_update
module Guard = Helpers_guard
module Search = Helpers_search
module MessageTemplates = Helpers_message_templates
module QueueJobs = Helpers_queue_jobs

module Access : sig
  val index : Rock.Middleware.t
  val create : Rock.Middleware.t
  val read : Rock.Middleware.t
  val update : Rock.Middleware.t
  val delete : Rock.Middleware.t
end = struct
  module Guardian = Middleware.Guardian

  let index = Guardian.denied
  let create = Guardian.denied
  let read = Guardian.denied
  let update = Guardian.denied
  let delete = Guardian.denied
end

let terms_and_conditions_accepted urlencoded =
  let open Pool_message in
  CCList.assoc ~eq:( = ) Field.(show TermsAccepted) urlencoded
  |> CCList.hd
  |> CCString.equal "true"
  |> Utils.Bool.to_result Error.TermsAndConditionsNotAccepted
  |> Lwt_result.lift
;;
