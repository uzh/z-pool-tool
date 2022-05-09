module SignUp = Subject_signup
module UserProfile = Subject_user_profile
module Experiment = Subject_experiment
module Command = Cqrs_command.Subject_command
module HttpUtils = Http_utils

let dashboard req =
  let result context =
    let open Lwt_result.Infix in
    Page.Subject.dashboard context
    |> Subject_general.create_layout req ~active_navigation:"/dashboard" context
    >|= Sihl.Web.Response.of_html
    |> Lwt_result.map_err (fun err -> err, "/index")
  in
  result |> HttpUtils.extract_happy_path req
;;
