module User = Pool_user
module Common = Pool_common
module Database = Database
open Entity

let src = Logs.Src.create "admin.event"

type create =
  { id : Id.t option
  ; email : User.EmailAddress.t
  ; password : User.Password.Plain.t [@opaque]
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

type event =
  | Created of create
  | DetailsUpdated of t * update
  | EmailVerified of t
  | Disabled of t
  | Enabled of t
  | ImportConfirmed of t * User.Password.Plain.t
  | ImportDisabled of t
  | PromotedContact of Pool_user.Id.t
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
      let id = CCOption.map Id.to_user id in
      User.create_admin
        pool
        ?id
        email
        lastname
        firstname
        password
        (Pool_user.Password.to_confirmed password)
      >|+ create ~email_verified:None
      >|- with_log_error ~src ~tags
      ||> get_or_failwith
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
    let%lwt (_ : Pool_user.t) = User.update pool ~lastname ~firstname (user admin) in
    Lwt.return_unit
  | EmailVerified admin ->
    let%lwt (_ : Pool_user.t) = User.confirm pool admin.user in
    { admin with email_verified = Some (Pool_user.EmailVerified.create_now ()) }
    |> Repo.update pool
  | ImportConfirmed (admin, password) ->
    let (_ : (unit, Pool_message.Error.t) Lwt_result.t) =
      User.Password.define
        pool
        (admin |> user |> Pool_user.id)
        password
        (Pool_user.Password.to_confirmed password)
      >|- Pool_common.Utils.with_log_error ~src ~tags
    in
    Repo.update pool { admin with import_pending = Pool_user.ImportPending.create false }
  | ImportDisabled admin ->
    Repo.update pool { admin with import_pending = Pool_user.ImportPending.create false }
  | PromotedContact contact_id ->
    let target =
      Guard.Persistence.Target.find
        ~ctx
        (Guard.Uuid.target_of Pool_user.Id.value contact_id)
    in
    let default = Lwt.return_unit in
    target
    >|- Pool_message.Error.nothandled
    >|> map_or ~tags ~default (fun { Guard.Target.uuid; _ } ->
      let%lwt () = Repo.promote_contact pool contact_id in
      let%lwt () = Guard.Persistence.Target.promote ~ctx uuid `Admin in
      let%lwt () =
        Repo.find pool (Id.of_user contact_id)
        >|> map_or ~tags ~default admin_authorizable
      in
      Lwt.return_unit)
  | SignInCounterUpdated m -> Repo.update_sign_in_count pool m
  | Disabled _ -> Utils.todo ()
  | Enabled _ -> Utils.todo ()
[@@deriving eq, show]
;;

let show_event = Format.asprintf "%a" pp_event
