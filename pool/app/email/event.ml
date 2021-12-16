open Entity
module User = Pool_user

let create_token pool address =
  let open Lwt.Infix in
  Service.Token.create
    ~ctx:(Pool_tenant.to_ctx pool)
    [ "email", User.EmailAddress.value address ]
  >|= Token.create
;;

let deactivate_token pool token =
  Service.Token.deactivate ~ctx:(Pool_tenant.to_ctx pool) token
;;

let send_confirmation_email pool email firstname lastname event =
  let open Lwt.Infix in
  Helper.ConfirmationEmail.create pool email firstname lastname event
  >>= Service.Email.send ~ctx:(Pool_tenant.to_ctx pool)
;;

type event =
  | Created of
      User.EmailAddress.t
      * Pool_common.Id.t
      * User.Firstname.t
      * User.Lastname.t
  | Updated of
      User.EmailAddress.t
      * Pool_common.Id.t
      * User.Firstname.t
      * User.Lastname.t
  | EmailVerified of unverified t

let handle_event pool : event -> unit Lwt.t =
  let open Lwt.Infix in
  let create_email user_id address firstname lastname event : unit Lwt.t =
    create_token pool address
    >|= create address user_id
    >>= fun email ->
    let%lwt () = Repo.insert pool email in
    send_confirmation_email pool email firstname lastname event
  in
  function
  | Created (address, user_id, firstname, lastname) ->
    let%lwt () = Repo.delete_unverified_by_user pool user_id in
    create_email user_id address firstname lastname `SignUp
  | Updated (address, user_id, firstname, lastname) ->
    let%lwt () = Repo.delete_unverified_by_user pool user_id in
    create_email user_id address firstname lastname `EmailUpdate
  | EmailVerified (Unverified { token; _ } as email) ->
    let%lwt () = deactivate_token pool token in
    let%lwt () = Repo.update pool @@ verify email in
    Lwt.return_unit
;;

let[@warning "-4"] equal_event (one : event) (two : event) : bool =
  match one, two with
  | Created (a1, id1, f1, l1), Created (a2, id2, f2, l2) ->
    User.EmailAddress.equal a1 a2
    && Pool_common.Id.equal id1 id2
    && User.Firstname.equal f1 f2
    && User.Lastname.equal l1 l2
  | Updated (a1, id1, f1, l1), Updated (a2, id2, f2, l2) ->
    User.EmailAddress.equal a1 a2
    && Pool_common.Id.equal id1 id2
    && User.Firstname.equal f1 f2
    && User.Lastname.equal l1 l2
  | EmailVerified m, EmailVerified p -> equal m p
  | _ -> false
;;

let pp_event formatter (event : event) : unit =
  let pp_address = User.EmailAddress.pp formatter in
  match event with
  | Created (m, id, f, l) | Updated (m, id, f, l) ->
    pp_address m;
    Pool_common.Id.pp formatter id;
    User.Firstname.pp formatter f;
    User.Lastname.pp formatter l
  | EmailVerified m -> pp formatter m
;;
