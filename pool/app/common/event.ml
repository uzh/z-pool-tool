open Entity

module Email = struct
  type event =
    | Created of Email.Address.t
    | UpdatedUnverified of Email.unverified Email.t * Email.Address.t
    | UpdatedVerified of Email.verified Email.t * Email.Address.t
    | Verified of Email.unverified Email.t

  let handle_event : event -> unit Lwt.t =
    let open Lwt.Syntax in
    let create_email address =
      let* token =
        Service.Token.create [ "email", Email.Address.show address ]
      in
      Repo.Email.insert @@ Email.Unverified { address; token }
    in
    function
    | Created address -> create_email address
    | UpdatedUnverified (Unverified email, new_address) ->
      let* () = Service.Token.deactivate email.token in
      create_email new_address
    | UpdatedVerified (Verified _, new_address) -> create_email new_address
    | Verified (Unverified { token; _ } as email) ->
      let* () = Service.Token.deactivate token in
      Repo.Email.update @@ Email.verify email
  ;;
end
