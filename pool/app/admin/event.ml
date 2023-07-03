module User = Pool_user
module Common = Pool_common
module Database = Pool_database
open Entity

let src = Logs.Src.create "admin.event"

type create =
  { id : Id.t option
  ; email : User.EmailAddress.t
  ; password : User.Password.t [@opaque]
  ; firstname : User.Firstname.t
  ; lastname : User.Lastname.t
  ; roles : Guard.RoleSet.t option
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
 fun pool { user } password password_confirmation ->
  let open Lwt_result.Infix in
  Service.User.set_password
    ~ctx:(Pool_database.to_ctx pool)
    user
    ~password
    ~password_confirmation
  >|= ignore
;;

type event =
  | Created of create
  | DetailsUpdated of t * update
  | PasswordUpdated of
      t * User.Password.t * User.Password.t * User.PasswordConfirmed.t
  | Disabled of t
  | Enabled of t
  | Verified of t
[@@deriving eq, show]

let handle_event ~tags pool : event -> unit Lwt.t =
  let open Utils.Lwt_result.Infix in
  function
  | Created { id; lastname; firstname; password; email; roles } ->
    let open Pool_common.Utils in
    let ctx = Pool_database.to_ctx pool in
    let%lwt admin =
      Service.User.create_admin
        ~ctx
        ?id:(id |> CCOption.map Pool_common.Id.value)
        ~name:(lastname |> User.Lastname.value)
        ~given_name:(firstname |> User.Firstname.value)
        ~password:(password |> User.Password.to_sihl)
        (User.EmailAddress.value email)
      ||> create
    in
    let%lwt () = Repo.insert pool admin in
    let%lwt (_ : Role.Target.t Guard.Target.t) =
      Entity_guard.Target.to_authorizable ~ctx admin
      >|- with_log_error ~src ~tags
      ||> get_or_failwith
    in
    let%lwt (authorizable : Role.Actor.t Guard.Actor.t) =
      Entity_guard.Actor.to_authorizable ~ctx admin
      >|- with_log_error ~src ~tags
      ||> get_or_failwith
    in
    let%lwt () =
      match roles with
      | Some roles ->
        Guard.Persistence.Actor.grant_roles
          ~ctx
          (authorizable |> Guard.Actor.id)
          roles
        ||> CCResult.get_or_failwith
      | None -> Lwt.return_unit
    in
    Lwt.return_unit
  | DetailsUpdated (admin, { firstname; lastname }) ->
    let name = User.Lastname.value lastname in
    let given_name = User.Firstname.value firstname in
    let%lwt (_ : Sihl_user.t) =
      Service.User.update
        ~ctx:(Pool_database.to_ctx pool)
        ~name
        ~given_name
        (user admin)
    in
    Lwt.return_unit
  | PasswordUpdated (admin, old_password, new_password, confirmed) ->
    let old_password = old_password |> User.Password.to_sihl in
    let new_password = new_password |> User.Password.to_sihl in
    let new_password_confirmation =
      confirmed |> User.PasswordConfirmed.to_sihl
    in
    let%lwt (_ : (Sihl_user.t, string) result) =
      let open Pool_common in
      Service.User.update_password
        ~ctx:(Pool_database.to_ctx pool)
        ~password_policy:(CCFun.const (CCResult.return ()))
        ~old_password
        ~new_password
        ~new_password_confirmation
        (user admin)
      ||> Utils.with_log_result_error ~src ~tags Message.nothandled
    in
    Lwt.return_unit
  | Disabled _ -> Utils.todo ()
  | Enabled _ -> Utils.todo ()
  | Verified _ -> Utils.todo ()
  [@@deriving eq, show]
;;

let show_event = Format.asprintf "%a" pp_event
