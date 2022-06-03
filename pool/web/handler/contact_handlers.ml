module SignUp = Contact_signup
module UserProfile = Contact_user_profile
module Experiment = Contact_experiment
module WaitingList = Contact_waiting_list
module Session = Contact_session
module Assignment = Contact_assignment
module Command = Cqrs_command.Contact_command
module HttpUtils = Http_utils

let dashboard req =
  let result context =
    let open Lwt_result.Infix in
    Page.Contact.dashboard context
    |> Contact_general.create_layout req ~active_navigation:"/dashboard" context
    >|= Sihl.Web.Response.of_html
    |> Lwt_result.map_err (fun err -> err, "/index")
  in
  result |> HttpUtils.extract_happy_path req
;;
