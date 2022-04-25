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

let send_confirmation_email pool language email firstname lastname event =
  let open Lwt.Infix in
  Helper.ConfirmationEmail.create pool language email firstname lastname event
  >>= Service.Email.send ~ctx:(Pool_tenant.to_ctx pool)
;;

type event =
  | Created of
      User.EmailAddress.t
      * Pool_common.Id.t
      * User.Firstname.t
      * User.Lastname.t
      * Pool_common.Language.t
  | Updated of User.EmailAddress.t * Sihl_user.t * Pool_common.Language.t
  | EmailVerified of unverified t
  | DefaultRestored of Default.default

let handle_event pool : event -> unit Lwt.t =
  let open Lwt.Infix in
  let create_email language user_id address firstname lastname label
      : unit Lwt.t
    =
    let ctx = Pool_tenant.to_ctx pool in
    let%lwt token = create_token pool address in
    user_id
    |> Pool_common.Id.value
    |> Service.User.find_opt ~ctx
    >>= function
    | None ->
      let error = PoolError.(NotFound Field.User |> show_error) in
      Logs.err (fun m -> m "Cannot create verification email: %s" error);
      failwith error
    | Some user ->
      let email = create address user token in
      let%lwt () = Repo.insert pool email in
      send_confirmation_email pool language email firstname lastname label
  in
  function
  | Created (address, user_id, firstname, lastname, language) ->
    let%lwt () = Repo.delete_unverified_by_user pool user_id in
    create_email
      language
      user_id
      address
      (Some firstname)
      (Some lastname)
      TemplateLabel.SignUpVerification
  | Updated (address, user, language) ->
    let open Sihl.Contract.User in
    let user_id = user.id |> Pool_common.Id.of_string in
    let%lwt () = Repo.delete_unverified_by_user pool user_id in
    create_email
      language
      user_id
      address
      (user.given_name |> CCOption.map Pool_user.Firstname.of_string)
      (user.name |> CCOption.map Pool_user.Lastname.of_string)
      TemplateLabel.EmailVerification
  | EmailVerified (Unverified { token; _ } as email) ->
    let%lwt () = deactivate_token pool token in
    let%lwt () = Repo.verify pool @@ verify email in
    Lwt.return_unit
  | DefaultRestored default_values ->
    Lwt_list.iter_s
      (fun { Default.label; language; text; html } ->
        let%lwt () = Repo.delete_email_template pool label language in
        let%lwt _ =
          Service.EmailTemplate.create
            ~ctx:(Pool_tenant.to_ctx pool)
            ~label:(TemplateLabel.show label)
            ~language:(Pool_common.Language.code language)
            ~html
            text
        in
        Lwt.return_unit)
      default_values
;;

let[@warning "-4"] equal_event (one : event) (two : event) : bool =
  match one, two with
  | Created (a1, id1, f1, l1, _), Created (a2, id2, f2, l2, _) ->
    User.EmailAddress.equal a1 a2
    && Pool_common.Id.equal id1 id2
    && User.Firstname.equal f1 f2
    && User.Lastname.equal l1 l2
  | Updated (a1, u1, _), Updated (a2, u2, _) ->
    User.EmailAddress.equal a1 a2
    && CCString.equal u1.Sihl.Contract.User.id u2.Sihl.Contract.User.id
  | EmailVerified m, EmailVerified p -> equal m p
  | DefaultRestored one, DefaultRestored two -> Default.equal_default one two
  | _ -> false
;;

let pp_event formatter (event : event) : unit =
  let pp_address = User.EmailAddress.pp formatter in
  match event with
  | Created (m, id, f, l, language) ->
    pp_address m;
    Pool_common.Id.pp formatter id;
    User.Firstname.pp formatter f;
    User.Lastname.pp formatter l;
    Pool_common.Language.pp formatter language
  | Updated (m, u, language) ->
    pp_address m;
    Sihl_user.pp formatter u;
    Pool_common.Language.pp formatter language
  | EmailVerified m -> pp formatter m
  | DefaultRestored m -> Default.pp_default formatter m
;;
