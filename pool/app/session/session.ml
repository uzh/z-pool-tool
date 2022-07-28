include Entity
include Event

let find_all_for_experiment = Repo.find_all_for_experiment
let find_all_public_for_experiment = Repo.find_all_public_for_experiment
let find_all_public_by_location = Repo.find_all_public_by_location
let find = Repo.find
let find_public = Repo.find_public
let find_public_by_assignment = Repo.find_public_by_assignment
let find_experiment_id_and_title = Repo.find_experiment_id_and_title
let find_sessions_to_remind = Repo.find_sessions_to_remind
let find_follow_ups = Repo.find_follow_ups

(* TODO [aerben] should building be in cqrs command or in event *)
let build_cancellation_messages _tenant_db _language _contacts =
  (*   let open Email in *)
  (*   let%lwt template = *)
  (*     Service.EmailTemplate.get_by_label *)
  (*       ~ctx:(Pool_tenant.to_ctx tenant_db) *)
  (*       ~language:(Pool_common.Language.show language) *)
  (*       TemplateLabel.(show SessionCancellation) *)
  (*   in *)
  (*   let subject = *)
  (* Pool_common.(Utils.text_to_string language
     I18n.SessionCancellationSubject) *)
  (*   in *)
  (* let mail = Sihl_email.create *)
  (* let mail = Sihl_email.Template.render_email_with_data *)
  (* let open CCList.Infix in *)
  (* let* contact = contacts in *)
  Lwt_result.return []
;;
