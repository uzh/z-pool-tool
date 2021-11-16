open Entity

module Email = struct
  module PasswordReset = Helper.Email.PasswordReset

  let create_token pool address =
    let open Lwt.Infix in
    Service.Token.create
      ~ctx:(Pool_common.Utils.pool_to_ctx pool)
      [ "email", Email.Address.value address ]
    >|= Email.Token.create
  ;;

  let deactivate_token pool token =
    Service.Token.deactivate ~ctx:(Pool_common.Utils.pool_to_ctx pool) token
  ;;

  let send_confirmation_email pool email firstname lastname =
    let open Lwt.Infix in
    Helper.Email.ConfirmationEmail.create pool email firstname lastname
    >>= Service.Email.send ~ctx:(Pool_common.Utils.pool_to_ctx pool)
  ;;

  type event =
    | Created of Email.Address.t * Firstname.t * Lastname.t
    | UpdatedUnverified of
        Email.unverified Email.t * (Email.Address.t * Firstname.t * Lastname.t)
    | UpdatedVerified of
        Email.verified Email.t * (Email.Address.t * Firstname.t * Lastname.t)
    | Verified of Email.unverified Email.t

  let handle_event pool : event -> unit Lwt.t =
    let open Lwt.Infix in
    let create_email address firstname lastname : unit Lwt.t =
      create_token pool address
      >|= Email.create address
      >>= fun email ->
      let%lwt () = Repo.Email.insert pool email in
      send_confirmation_email pool email firstname lastname
    in
    let update_email old_email new_address firstname lastname =
      create_token pool new_address
      >|= Email.create new_address
      >>= fun new_email ->
      let%lwt () = Repo.Email.update_email pool old_email new_email in
      send_confirmation_email pool new_email firstname lastname
    in
    function
    | Created (address, firstname, lastname) ->
      create_email address firstname lastname
    | UpdatedUnverified
        ( (Email.(Unverified { token; _ }) as old_email)
        , (new_address, firstname, lastname) ) ->
      let%lwt () = deactivate_token pool token in
      update_email old_email new_address firstname lastname
    | UpdatedVerified
        ((Email.Verified _ as old_email), (new_address, firstname, lastname)) ->
      update_email old_email new_address firstname lastname
    | Verified (Email.(Unverified { token; _ }) as email) ->
      let%lwt () = deactivate_token pool token in
      let%lwt () = Repo.Email.update pool @@ Email.verify email in
      Lwt.return_unit
  ;;

  let[@warning "-4"] equal_event (one : event) (two : event) : bool =
    match one, two with
    | Created (a1, f1, l1), Created (a2, f2, l2) ->
      Email.Address.equal a1 a2 && Firstname.equal f1 f2 && Lastname.equal l1 l2
    | UpdatedUnverified (m1, (a1, f1, l1)), UpdatedUnverified (m2, (a2, f2, l2))
      ->
      Email.equal m1 m2
      && Email.Address.equal a1 a2
      && Firstname.equal f1 f2
      && Lastname.equal l1 l2
    | UpdatedVerified (m1, (a1, f1, l1)), UpdatedVerified (m2, (a2, f2, l2)) ->
      Email.equal m1 m2
      && Email.Address.equal a1 a2
      && Firstname.equal f1 f2
      && Lastname.equal l1 l2
    | Verified m, Verified p -> Email.equal m p
    | _ -> false
  ;;

  let pp_event formatter (event : event) : unit =
    let pp_address = Email.Address.pp formatter in
    match event with
    | Created (m, f, l) ->
      pp_address m;
      Firstname.pp formatter f;
      Lastname.pp formatter l
    | UpdatedUnverified (m, (a, f, l)) ->
      Email.pp formatter m;
      pp_address a;
      Firstname.pp formatter f;
      Lastname.pp formatter l
    | UpdatedVerified (m, (a, f, l)) ->
      Email.pp formatter m;
      pp_address a;
      Firstname.pp formatter f;
      Lastname.pp formatter l
    | Verified m -> Email.pp formatter m
  ;;
end
