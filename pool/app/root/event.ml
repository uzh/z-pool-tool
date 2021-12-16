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

let handle_event pool : event -> unit Lwt.t =
  let ctx = Tenant_pool.to_ctx pool in
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
    Permission.assign user Role.root
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

let[@warning "-4"] equal_event event1 event2 : bool =
  let open Sihl.Contract.User in
  match event1, event2 with
  | Created one, Created two -> equal_create one two
  | Disabled r1, Disabled r2 | Enabled r1, Enabled r2 ->
    CCString.equal r1.id r2.id
  | _ -> false
;;

let pp_event formatter event =
  match event with
  | Created create -> pp_create formatter create
  | Disabled m | Enabled m -> Sihl_user.pp formatter m
;;
