open Utils.Lwt_result.Infix
open Pool_message
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Response = Http_response

let src = Logs.Src.create "handler.admin.contacts.tags"
let contact_id = HttpUtils.find_id Contact.Id.of_string Field.Contact
let contact_path = HttpUtils.Url.Admin.contact_path

let handle_tag action req =
  let tags = Pool_context.Logger.Tags.req req in
  let contact_id = contact_id req in
  let path = contact_path ~id:contact_id ~suffix:"edit" () in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
  in
  let result { Pool_context.database_label; user; _ } =
    let* contact =
      Contact.find database_label contact_id |> Response.not_found_on_error
    in
    Response.bad_request_on_error ~urlencoded Admin_contacts.edit
    @@ let* message, events =
         match action with
         | `Assign ->
           let open Cqrs_command.Tags_command.AssignTagToContact in
           urlencoded
           |> decode
           |> Lwt_result.lift
           >== handle ~tags contact
           >|+ CCPair.make Success.TagAssigned
         | `Remove ->
           let open Cqrs_command.Tags_command.RemoveTagFromContact in
           HttpUtils.find_id Tags.Id.of_string Field.Tag req
           |> Tags.find database_label
           >== handle contact
           >|+ CCPair.make Success.TagRemoved
       in
       let handle = Lwt_list.iter_s (Pool_event.handle_event ~tags database_label user) in
       let return_to_overview () =
         HttpUtils.redirect_to_with_actions path [ Message.set ~success:[ message ] ]
       in
       events |> handle >|> return_to_overview |> Lwt_result.ok
  in
  Response.handle ~src req result
;;

let assign_tag = handle_tag `Assign
let remove_tag = handle_tag `Remove
