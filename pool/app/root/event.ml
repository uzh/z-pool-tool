open Entity
module User = Pool_user
module Common = Pool_common

type create =
  { email : User.EmailAddress.t
  ; password : User.Password.t
  ; firstname : User.Firstname.t
  ; lastname : User.Lastname.t
  }
[@@deriving eq, show]

type event =
  | Created of create
  | Disabled of t
  | Enabled of t
[@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t =
  let ctx = Pool_tenant.to_ctx pool in
  function
  | Created root ->
    let%lwt user =
      Service.User.create_admin
        ~ctx
        ~name:(root.lastname |> User.Lastname.value)
        ~given_name:(root.firstname |> User.Firstname.value)
        ~password:(root.password |> User.Password.to_sihl)
        (User.EmailAddress.value root.email)
    in
    let%lwt rv =
      Guard.Persistence.Actor.grant_roles
        (Guard.Uuid.Actor.of_string_exn user.Sihl.Contract.User.id)
        (Guard.ActorRoleSet.singleton `Admin)
    in
    Lwt.return (CCResult.get_exn rv)
  | Disabled root ->
    let%lwt _ =
      Sihl_user.
        { root with status = Inactive; updated_at = Common.UpdatedAt.create () }
      |> Service.User.update ~ctx
    in
    Lwt.return_unit
  | Enabled root ->
    let%lwt _ =
      Sihl_user.
        { root with status = Active; updated_at = Common.UpdatedAt.create () }
      |> Service.User.update ~ctx
    in
    Lwt.return_unit
;;
