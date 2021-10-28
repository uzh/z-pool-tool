type request = Rock.Request.t
type response = Rock.Response.t
type handler = request -> response Lwt.t

let urlencoded_of_request = Utils.todo
let response_of_redirect = Utils.todo
let response = Utils.todo
let handle_events _ = Utils.todo
