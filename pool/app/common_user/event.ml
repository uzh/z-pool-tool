open Entity

module Email = struct
  type event =
    | Created of Email.Address.t
    | UpdatedUnverified of Email.unverified Email.t * Email.Address.t
    | UpdatedVerified of Email.verified Email.t * Email.Address.t
    | Verified of Email.unverified Email.t

  let handle_event pool : event -> unit Lwt.t =
    let open Lwt.Infix in
    let create_email address =
      let%lwt token =
        Service.Token.create
          ~ctx:[ "pool", Pool_common.Database.Label.value pool ]
          [ "email", Email.Address.show address ]
        >|= Email.Token.create
      in
      Repo.Email.insert pool @@ Email.create address token
    in
    function
    | Created address -> create_email address
    | UpdatedUnverified (Email.Unverified email, new_address) ->
      let%lwt () = Service.Token.deactivate email.Email.token in
      create_email new_address
    | UpdatedVerified (Email.Verified _, new_address) ->
      create_email new_address
    | Verified (Email.(Unverified { token; _ }) as email) ->
      let%lwt () = Service.Token.deactivate token in
      Repo.Email.update pool @@ Email.verify email
  ;;

  let[@warning "-4"] equal_event (one : event) (two : event) : bool =
    match one, two with
    | Created m, Created p -> Email.Address.equal m p
    | UpdatedUnverified (m1, p1), UpdatedUnverified (m2, p2) ->
      Email.equal m1 m2 && Email.Address.equal p1 p2
    | UpdatedVerified (m1, p1), UpdatedVerified (m2, p2) ->
      Email.equal m1 m2 && Email.Address.equal p1 p2
    | Verified m, Verified p -> Email.equal m p
    | _ -> false
  ;;

  let pp_event formatter (event : event) : unit =
    let pp_address = Email.Address.pp formatter in
    match event with
    | Created m -> pp_address m
    | UpdatedUnverified (m, p) ->
      Email.pp formatter m;
      pp_address p
    | UpdatedVerified (m, p) ->
      Email.pp formatter m;
      pp_address p
    | Verified m -> Email.pp formatter m
  ;;
end
