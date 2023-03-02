module User = Pool_user
module Common = Pool_common
module Database = Pool_database
open Entity

type create =
  { id : Pool_common.Id.t option
  ; email : User.EmailAddress.t
  ; password : User.Password.t [@opaque]
  ; firstname : User.Firstname.t
  ; lastname : User.Lastname.t
  ; roles : Guard.ActorRoleSet.t option
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
 fun pool user password password_confirmation ->
  let open Lwt_result.Infix in
  Service.User.set_password
    ~ctx:(Pool_tenant.to_ctx pool)
    user
    ~password
    ~password_confirmation
  >|= ignore
;;

type event =
  | Created of create
  | DetailsUpdated of t * update
  | PasswordUpdated of t * User.Password.t * User.PasswordConfirmed.t
  | Disabled of t
  | Enabled of t
  | Verified of t
[@@deriving eq, show]

let handle_event ~tags pool : event -> unit Lwt.t =
  let open Utils.Lwt_result.Infix in
  function
  | Created { id; lastname; firstname; password; email; roles } ->
    let ctx = Pool_tenant.to_ctx pool in
    let%lwt user =
      Service.User.create_admin
        ~ctx
        ?id:(id |> CCOption.map Pool_common.Id.value)
        ~name:(lastname |> User.Lastname.value)
        ~given_name:(firstname |> User.Firstname.value)
        ~password:(password |> User.Password.to_sihl)
        (User.EmailAddress.value email)
    in
    let%lwt (authorizable : [> `Admin ] Guard.Authorizable.t) =
      Entity_guard.Actor.to_authorizable ~ctx user
      >|- Pool_common.Utils.with_log_error ~tags
      ||> Pool_common.Utils.get_or_failwith
    in
    let%lwt () =
      match roles with
      | Some roles ->
        Guard.Persistence.Actor.grant_roles
          ~ctx
          authorizable.Guard.Authorizable.uuid
          roles
        ||> CCResult.get_or_failwith
      | None -> Lwt.return_unit
    in
    Lwt.return_unit
  | DetailsUpdated (_, _) -> Lwt.return_unit
  | PasswordUpdated (person, password, confirmed) ->
    let%lwt (_ : (unit, string) result) =
      set_password
        pool
        person
        (password |> User.Password.to_sihl)
        (confirmed |> User.PasswordConfirmed.to_sihl)
      ||> Pool_common.(Utils.with_log_result_error ~tags Message.nothandled)
    in
    Lwt.return_unit
  | Disabled _ -> Utils.todo ()
  | Enabled _ -> Utils.todo ()
  | Verified _ -> Utils.todo ()
  [@@deriving eq, show]
;;

let show_event = Format.asprintf "%a" pp_event
