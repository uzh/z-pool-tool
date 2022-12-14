include Entity
include Event
module Guard = Entity_guard

let find_all_for_experiment = Repo.find_all_for_experiment
let find_all_public_for_experiment = Repo.find_all_public_for_experiment
let find_all_public_by_location = Repo.find_all_public_by_location
let find = Repo.find
let find_public = Repo.find_public
let find_public_by_assignment = Repo.find_public_by_assignment
let find_by_assignment = Repo.find_by_assignment
let find_experiment_id_and_title = Repo.find_experiment_id_and_title
let find_sessions_to_remind = Repo.find_sessions_to_remind
let find_follow_ups = Repo.find_follow_ups

let build_cancellation_messages
  tenant
  tenant_db
  language
  system_languages
  session
  contacts
  =
  let open Utils.Lwt_result.Infix in
  let find = CCFun.flip (I18n.find_by_key tenant_db) language in
  (* TODO make these exceptions, reevaluate when to use them *)
  let* subject = find I18n.Key.SessionCancellationSubject in
  let* text = find I18n.Key.SessionCancellationText in
  let layout = Email.Helper.layout_from_tenant tenant in
  let template =
    Email.CustomTemplate.
      { subject = Subject.I18n subject; content = Content.I18n text; layout }
  in
  let session_overview =
    (CCList.map (fun lang ->
       ( Format.asprintf "sessionOverview%s" (Pool_common.Language.show lang)
       , to_email_text lang session )))
      system_languages
  in
  (fun reason ->
    let create_message (contact : Contact.t) =
      Email.Helper.prepare_boilerplate_email
        template
        (contact |> Contact.email_address |> Pool_user.EmailAddress.value)
        ([ "name", Contact.fullname contact; "reason", reason ]
        @ session_overview)
    in
    CCList.map create_message contacts)
  |> Lwt_result.return
;;
