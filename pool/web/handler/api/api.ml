module OrganisationalUnit = Api_organisational_unit

let not_found (_ : Rock.Request.t) =
  Http_utils.Api.respond_error
    ~status:`Not_found
    Pool_message.(Error.NotFound Field.Resource)
  |> Lwt.return
;;
