open Entity
module User = Pool_user

let create_token pool address =
  let open Lwt.Infix in
  Service.Token.create
    ~ctx:(Tenant_pool.to_ctx pool)
    [ "email", User.EmailAddress.value address ]
  >|= Token.create
;;

let deactivate_token pool token =
  Service.Token.deactivate ~ctx:(Tenant_pool.to_ctx pool) token
;;

let send_signup_email pool email firstname lastname =
  let open Lwt.Infix in
  Helper.SignUp.create pool email firstname lastname
  >>= Service.Email.send ~ctx:(Tenant_pool.to_ctx pool)
;;

let send_confirmation_email pool email firstname lastname =
  let open Lwt.Infix in
  Helper.ConfirmationEmail.create pool email firstname lastname
  >>= Service.Email.send ~ctx:(Tenant_pool.to_ctx pool)
;;

type event =
  | Created of User.EmailAddress.t * User.Firstname.t * User.Lastname.t
  | UpdatedUnverified of
      unverified t * (User.EmailAddress.t * User.Firstname.t * User.Lastname.t)
  | UpdatedVerified of
      verified t * (User.EmailAddress.t * User.Firstname.t * User.Lastname.t)
  | EmailVerified of unverified t

let handle_event pool : event -> unit Lwt.t =
  let open Lwt.Infix in
  let create_email address firstname lastname : unit Lwt.t =
    create_token pool address
    >|= create address
    >>= fun email ->
    let%lwt () = Repo.insert pool email in
    send_signup_email pool email firstname lastname
  in
  let update_email old_email new_address firstname lastname =
    create_token pool new_address
    >|= create new_address
    >>= fun new_email ->
    let%lwt () = Repo.update_email pool old_email new_email in
    send_confirmation_email pool new_email firstname lastname
  in
  function
  | Created (address, firstname, lastname) ->
    create_email address firstname lastname
  | UpdatedUnverified
      ( (Unverified { token; _ } as old_email)
      , (new_address, firstname, lastname) ) ->
    let%lwt () = deactivate_token pool token in
    update_email old_email new_address firstname lastname
  | UpdatedVerified
      ((Verified _ as old_email), (new_address, firstname, lastname)) ->
    update_email old_email new_address firstname lastname
  | EmailVerified (Unverified { token; _ } as email) ->
    let%lwt () = deactivate_token pool token in
    let%lwt () = Repo.update pool @@ verify email in
    Lwt.return_unit
;;

let[@warning "-4"] equal_event (one : event) (two : event) : bool =
  match one, two with
  | Created (a1, f1, l1), Created (a2, f2, l2) ->
    User.EmailAddress.equal a1 a2
    && User.Firstname.equal f1 f2
    && User.Lastname.equal l1 l2
  | UpdatedUnverified (m1, (a1, f1, l1)), UpdatedUnverified (m2, (a2, f2, l2))
    ->
    equal m1 m2
    && User.EmailAddress.equal a1 a2
    && User.Firstname.equal f1 f2
    && User.Lastname.equal l1 l2
  | UpdatedVerified (m1, (a1, f1, l1)), UpdatedVerified (m2, (a2, f2, l2)) ->
    equal m1 m2
    && User.EmailAddress.equal a1 a2
    && User.Firstname.equal f1 f2
    && User.Lastname.equal l1 l2
  | EmailVerified m, EmailVerified p -> equal m p
  | _ -> false
;;

let pp_event formatter (event : event) : unit =
  let pp_address = User.EmailAddress.pp formatter in
  match event with
  | Created (m, f, l) ->
    pp_address m;
    User.Firstname.pp formatter f;
    User.Lastname.pp formatter l
  | UpdatedUnverified (m, (a, f, l)) ->
    pp formatter m;
    pp_address a;
    User.Firstname.pp formatter f;
    User.Lastname.pp formatter l
  | UpdatedVerified (m, (a, f, l)) ->
    pp formatter m;
    pp_address a;
    User.Firstname.pp formatter f;
    User.Lastname.pp formatter l
  | EmailVerified m -> pp formatter m
;;
