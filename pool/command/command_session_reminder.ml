let[@warning "-26"] [@warning "-27"] create_reminder
    pool
    language
    email
    contact
    text
  =
  let name = Contact.fullname contact in
  let subject = "Password has been changed" in
  failwith "Todo"
;;

(* Email.Helper.prepare_email pool language TemplateLabel.PasswordChange subject
   (email |> Pool_user.EmailAddress.value) [ "name", name ] *)

(* TODO[timhub]: How to deal with reminder text and languages? *)
let send_reminder pool session =
  let open Lwt_result.Syntax in
  let sys_languages = Pool_common.Language.all () in
  let default_texts =
    Hashtbl.create ~random:true (CCList.length sys_languages)
  in
  let* _ = Experiment.find_of_session pool session.Session.id in
  let* assignments =
    Assignment.find_uncanceled_by_session pool session.Session.id
  in
  let* default_language = Settings.default_language pool in
  Lwt_result.ok
  @@ Lwt_list.map_s
       (fun (assignment : Assignment.t) ->
         let contact = assignment.Assignment.contact in
         let message_language =
           CCOption.value ~default:default_language contact.Contact.language
         in
         let* text =
           match Hashtbl.find_opt default_texts message_language with
           | Some text -> Lwt_result.ok text
           | None ->
            (*Add to hashtable*) I18n.(find_by_key pool Key.ReminderText message_language)
         in
         Lwt_result.ok
         @@ create_reminder pool message_language "Template" contact text)
       assignments
;;
