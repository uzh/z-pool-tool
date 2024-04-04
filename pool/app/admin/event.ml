module User = Pool_user
module Common = Pool_common
module Database = Database
open Entity

let src = Logs.Src.create "admin.event"

type create =
  { id : Id.t option
  ; email : User.EmailAddress.t
  ; password : User.Password.t [@opaque]
  ; firstname : User.Firstname.t
  ; lastname : User.Lastname.t
  ; roles : (Role.Role.t * Guard.Uuid.Target.t option) list
  }
[@@deriving eq, show]

type update =
  { firstname : User.Firstname.t
  ; lastname : User.Lastname.t
  }
[@@deriving eq, show]

let set_password
  : Database.Label.t -> t -> string -> string -> (unit, string) result Lwt.t
  =
  fun pool { user; _ } password password_confirmation ->
  let open Lwt_result.Infix in
  Service.User.set_password
    ~ctx:(Database.to_ctx pool)
    user
    ~password
    ~password_confirmation
  >|= ignore
;;

type event =
  | Created of create
  | DetailsUpdated of t * update
  | EmailVerified of t
  | Disabled of t
  | Enabled of t
  | ImportConfirmed of t * User.Password.t
  | ImportDisabled of t
  | PasswordUpdated of
      t * User.Password.t * User.Password.t * User.PasswordConfirmed.t
  | PromotedContact of Common.Id.t
  | SignInCounterUpdated of t
[@@deriving eq, show]

let map_or ~tags ~default fcn =
  let open Common in
  function
  | Error err ->
    let (_ : Pool_message.Error.t) = Utils.with_log_error ~src ~tags err in
    default
  | Ok value -> fcn value
;;

let handle_event ~tags pool : event -> unit Lwt.t =
  let open Utils.Lwt_result.Infix in
  let ctx = Database.to_ctx pool in
  let admin_authorizable ?(roles = []) admin =
    let open Guard in
    let open Common.Utils in
    let%lwt (authorizable : Actor.t) =
      Entity_guard.Actor.to_authorizable ~ctx admin
      >|- with_log_error ~src ~tags
      ||> get_or_failwith
    in
    Lwt_list.iter_s
      (fun (role, target_uuid) ->
        ActorRole.create ?target_uuid authorizable.Actor.uuid role
        |> Persistence.ActorRole.upsert ~ctx)
      roles
  in
  function
  | Created { id; lastname; firstname; password; email; roles } ->
    let open Common.Utils in
    let%lwt admin =
      Service.User.create_admin
        ~ctx
        ?id:(id |> CCOption.map Common.Id.value)
        ~name:(lastname |> User.Lastname.value)
        ~given_name:(firstname |> User.Firstname.value)
        ~password:(password |> User.Password.to_sihl)
        (User.EmailAddress.value email)
      ||> create ~email_verified:None
    in
    let%lwt () = Repo.insert pool admin in
    let () = Guard.Persistence.clear_cache () in
    let%lwt (_ : Guard.Target.t) =
      Entity_guard.Target.to_authorizable ~ctx admin
      >|- with_log_error ~src ~tags
      ||> get_or_failwith
    in
    admin_authorizable ~roles admin
  | DetailsUpdated (admin, { firstname; lastname }) ->
    let name = User.Lastname.value lastname in
    let given_name = User.Firstname.value firstname in
    let%lwt (_ : Sihl_user.t) =
      Service.User.update ~ctx ~name ~given_name (user admin)
    in
    Lwt.return_unit
  | EmailVerified admin ->
    let%lwt (_ : Sihl_user.t) =
      Service.User.update ~ctx Sihl_user.{ admin.user with confirmed = true }
    in
    { admin with email_verified = Some (Pool_user.EmailVerified.create_now ()) }
    |> Repo.update pool
  | ImportConfirmed (admin, password) ->
    let (_ : (Sihl_user.t Lwt.t, string) result) =
      let open Common in
      Service.User.set_user_password admin.user (User.Password.to_sihl password)
      |> CCResult.map (Service.User.update ~ctx)
      |> Utils.with_log_result_error ~src ~tags Pool_message.Error.nothandled
    in
    Repo.update
      pool
      { admin with import_pending = Pool_user.ImportPending.create false }
  | ImportDisabled admin ->
    Repo.update
      pool
      { admin with import_pending = Pool_user.ImportPending.create false }
  | PasswordUpdated (admin, old_password, new_password, confirmed) ->
    let old_password = old_password |> User.Password.to_sihl in
    let new_password = new_password |> User.Password.to_sihl in
    let new_password_confirmation =
      confirmed |> User.PasswordConfirmed.to_sihl
    in
    let%lwt (_ : (Sihl_user.t, string) result) =
      let open Common in
      Service.User.update_password
        ~ctx
        ~password_policy:(CCFun.const (CCResult.return ()))
        ~old_password
        ~new_password
        ~new_password_confirmation
        (user admin)
      ||> Utils.with_log_result_error ~src ~tags Pool_message.Error.nothandled
    in
    Lwt.return_unit
  | PromotedContact contact_id ->
    let target =
      Guard.Persistence.Target.find
        ~ctx
        (Guard.Uuid.target_of Id.value contact_id)
    in
    let default = Lwt.return_unit in
    target
    >|- Pool_message.Error.nothandled
    >|> map_or ~tags ~default (fun { Guard.Target.uuid; _ } ->
      let%lwt () = Repo.promote_contact pool contact_id in
      let%lwt () = Guard.Persistence.Target.promote ~ctx uuid `Admin in
      let%lwt () =
        Repo.find pool contact_id >|> map_or ~tags ~default admin_authorizable
      in
      Lwt.return_unit)
  | SignInCounterUpdated m -> Repo.update_sign_in_count pool m
  | Disabled _ -> Utils.todo ()
  | Enabled _ -> Utils.todo ()
[@@deriving eq, show]
;;

let show_event = Format.asprintf "%a" pp_event
