include Core
include Event
module Persistence = Repo
open Utils.Lwt_result.Infix

(** [console_authorizable] is an [\[ `Admin \] Authorizable.t] for use in
    administrative tasks, such as working with the command line or running
    tests. *)
let console_authorizable =
  Authorizable.make
    (ActorRoleSet.singleton `Admin)
    `Admin
    (Uuid.Actor.create ())
;;

(** [guest_authorizable] is a [\[ `Guest \] Authorizable.t] to be assigned to
    entities at the absolute lowest level of trust, such as users browsing the
    public facing website without logging in. *)
let guest_authorizable =
  Authorizable.make
    (ActorRoleSet.singleton `Guest)
    `Guest
    (Uuid.Actor.create ())
;;

(** Convenience module for turning Sihl or Pool [user]s into entities that
    [Guardian] can work with. *)
module User = struct
  type t = Sihl_user.t [@@deriving show]

  let to_authorizable t =
    Persistence.Actor.decorate
      (fun t ->
        Authorizable.make
          (ActorRoleSet.singleton `User)
          `User
          (Uuid.Actor.of_string_exn t.Sihl_user.id))
      t
    >|- Pool_common.Message.authorization
  ;;

  (** Many request handlers do not extract a [User.t] at any point. This
      function is useful in such cases. *)
  let authorizable_of_req ?ctx req =
    let* user_id =
      Sihl.Web.Session.find "user_id" req
      |> CCResult.of_opt
      |> CCResult.map_err (fun x -> Pool_common.Message.Authorization x)
      |> Lwt_result.lift
    in
    Persistence.Actor.decorate
      ?ctx
      (fun id ->
        Authorizable.make
          (ActorRoleSet.singleton `Contact)
          `Contact
          (Uuid.Actor.of_string_exn id))
      user_id
    >|- Pool_common.Message.authorization
  ;;
end

(** Convenience module for turning Pool [participant]s into entities that
    [Guardian] can work with. *)
module ContactTarget = struct
  type t = Contact.t [@@deriving show]

  let to_authorizable ?ctx t =
    Persistence.Target.decorate
      ?ctx
      (fun t ->
        AuthorizableTarget.make
          (TargetRoleSet.singleton `Contact)
          `Contact
          (Uuid.Target.of_string_exn (Pool_common.Id.value (Contact.id t))))
      t
    >|- (fun s ->
          Format.asprintf "Failed to convert Contact to authorizable: %s" s)
    >|- Pool_common.Message.authorization
  ;;
end

module Contact = struct
  type t = Contact.t [@@deriving show]

  let to_authorizable ?ctx t =
    Persistence.Actor.decorate
      ?ctx
      (fun t ->
        Authorizable.make
          (ActorRoleSet.singleton `Contact)
          `Contact
          (Uuid.Actor.of_string_exn (Pool_common.Id.value (Contact.id t))))
      t
    >|- (fun s ->
          Format.asprintf "Failed to convert Contact to authorizable: %s" s)
    >|- Pool_common.Message.authorization
  ;;
end

module PoolTenantTarget = struct
  type t = Pool_tenant.t [@@deriving show]

  let to_authorizable ?ctx t =
    Persistence.Target.decorate
      ?ctx
      (fun t ->
        AuthorizableTarget.make
          (TargetRoleSet.singleton `Tenant)
          `Tenant
          (Uuid.Target.of_string_exn (Pool_common.Id.value t.Pool_tenant.id)))
      t
    >|- Pool_common.Message.authorization
  ;;
end

module PoolTenant = struct
  type t = Pool_tenant.t [@@deriving show]

  let to_authorizable ?ctx t =
    Persistence.Actor.decorate
      ?ctx
      (fun t ->
        Authorizable.make
          (ActorRoleSet.singleton `Tenant)
          `Tenant
          (Uuid.Actor.of_string_exn (Pool_common.Id.value t.Pool_tenant.id)))
      t
    >|- Pool_common.Message.authorization
  ;;

  module Write = struct
    type t = Pool_tenant.Write.t

    let to_authorizable ?ctx t =
      Persistence.Actor.decorate
        ?ctx
        (fun t ->
          Authorizable.make
            (ActorRoleSet.singleton `Tenant)
            `User
            (Uuid.Actor.of_string_exn
               (Pool_common.Id.value t.Pool_tenant.Write.id)))
        t
      >|- Pool_common.Message.authorization
    ;;
  end
end

module Admin = struct
  type t

  (** converts an [authorizable] of type [\[ _ \] Authorizable.t] into a value
      of type [\[ `Admin \] Authorizable.t] if it possesses the appropriate
      [ `Admin ] role. *)
  let of_authorizable ?ctx (auth : _ Authorizable.t) =
    let* roles =
      Persistence.Actor.find_roles ?ctx auth.Authorizable.uuid
      >|- Pool_common.Message.authorization
    in
    if ActorRoleSet.mem `Admin roles
    then Lwt.return_ok { auth with Authorizable.typ = `Admin }
    else (
      Logs.err (fun m ->
        m
          "Entity %s cannot be treated as an `Admin"
          (Uuid.Actor.to_string auth.Authorizable.uuid));
      Lwt.return_error
        (Pool_common.Message.authorization
        @@ Format.asprintf
             "Entity %s cannot be treated as an `Admin"
             (Uuid.Actor.to_string auth.Authorizable.uuid)))
  ;;
end

(** The list of permissions that we need [Guardian] to be aware of in order to
    achieve a minimal level of functionality. Notably, the [`Admin] role should
    have [`Manage] authority on everything in the system. *)

(* TODO: check type, was: Authorizer.auth_rule list *)
let root_permissions : Authorizer.auth_rule list =
  CCList.map
    (fun role -> `ActorEntity `Admin, `Manage, `TargetEntity role)
    Role.Target.all
;;
