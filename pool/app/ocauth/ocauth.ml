include Core
module Persistence = Repo
open Utils.Lwt_result.Syntax

(** [console_authorizable] is an [\[ `Admin \] Authorizable.t] for use in
    administrative tasks, such as working with the command line or running
    tests. *)
let console_authorizable =
  Authorizable.make ~roles:(Role_set.singleton `Admin) ~typ:`Admin (Uuid.v `V4)
;;

(** [guest_authorizable] is a [\[ `Guest \] Authorizable.t] to be assigned to
    entities at the absolute lowest level of trust, such as users browsing the
    public facing website without logging in. *)
let guest_authorizable =
  Authorizable.make ~roles:(Role_set.singleton `Guest) ~typ:`Guest (Uuid.v `V4)
;;

(** Convenience module for turning Sihl or Pool [user]s into entities that
    [Guardian] can work with. *)
module User = struct
  type t = Sihl_user.t

  let to_authorizable t =
    Persistence.decorate_to_authorizable
      (fun t ->
        Authorizable.make
          ~roles:(Role_set.singleton `User)
          ~typ:`User
          (Uuid.of_string_exn t.Sihl_user.id))
      t
    |> Lwt_result.map_error Pool_common.Message.authorization
  ;;

  (** Many request handlers do not extract a [User.t] at any point. This
      function is useful in such cases. *)
  let authorizable_of_req req =
    let* user_id =
      Sihl.Web.Session.find "user_id" req
      |> CCResult.of_opt
      |> CCResult.map_err (fun x -> Pool_common.Message.Authorization x)
      |> Lwt_result.lift
    in
    Persistence.decorate_to_authorizable
      (fun id ->
        Authorizable.make
          ~roles:(Role_set.singleton `User)
          ~typ:`User
          (Uuid.of_string_exn id))
      user_id
    |> Lwt_result.map_error Pool_common.Message.authorization
  ;;
end

(** Convenience module for turning Pool [participant]s into entities that
    [Guardian] can work with. *)
module Contact = struct
  type t = Contact.t

  let to_authorizable t =
    Persistence.decorate_to_authorizable
      (fun t ->
        Authorizable.make
          ~roles:(Role_set.singleton `Contact)
          ~typ:`User
          (Uuid.of_string_exn (Pool_common.Id.value (Contact.id t))))
      t
    |> Lwt_result.map_error (fun s ->
         Format.asprintf "Failed to convert Contact to authorizable: %s" s)
    |> Lwt_result.map_error Pool_common.Message.authorization
  ;;
end

module Pool_tenant = struct
  type t = Pool_tenant.t

  let to_authorizable t =
    Persistence.decorate_to_authorizable
      (fun t ->
        Authorizable.make
          ~roles:(Role_set.singleton `Tenant)
          ~typ:`User
          (Uuid.of_string_exn (Pool_common.Id.value t.Pool_tenant.id)))
      t
    |> Lwt_result.map_error Pool_common.Message.authorization
  ;;

  module Write = struct
    type t = Pool_tenant.Write.t

    let to_authorizable t =
      Persistence.decorate_to_authorizable
        (fun t ->
          Authorizable.make
            ~roles:(Role_set.singleton `Tenant)
            ~typ:`User
            (Uuid.of_string_exn (Pool_common.Id.value t.Pool_tenant.Write.id)))
        t
      |> Lwt_result.map_error Pool_common.Message.authorization
    ;;
  end
end

module Admin = struct
  type t

  (** converts an [authorizable] of type [\[ _ \] Authorizable.t] into a value
      of type [\[ `Admin \] Authorizable.t] if it possesses the appropriate
      [ `Admin ] role. *)
  let of_authorizable (auth : _ Authorizable.t) =
    let* roles =
      Persistence.get_roles auth.Authorizable.uuid
      |> Lwt_result.map_error Pool_common.Message.authorization
    in
    if Role_set.mem `Admin roles
    then Lwt.return_ok { auth with Authorizable.typ = `Admin }
    else
      Lwt.return_error
        (Pool_common.Message.authorization
        @@ Format.asprintf
             "Entity %s cannot be treated as an `Admin"
             (Uuidm.to_string auth.Authorizable.uuid))
  ;;
end

(** The list of permissions that we need [Guardian] to be aware of in order to
    achieve a minimal level of functionality. Notably, the [`Admin] role should
    have [`Manage] authority on everything in the system. *)
let root_permissions : Authorizer.auth_rule list =
  CCList.map (fun role -> `Role `Admin, `Manage, `Role role) Role.all
;;
