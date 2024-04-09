open Sihl_user

module type Sig = sig
  module Web : sig
    (** [user_from_token ?key database_label read_token request] returns the user that is
        associated to the user id in the [Bearer] token of the [request].

        [key] is the key in the token associated with the user id. By default,
        the value is [user_id].

        [read_token] is a function that returns the associated value of [key] in
        a given token. *)
    val user_from_token
      :  ?key:string
      -> Database.Label.t
      -> (Database.Label.t -> string -> k:string -> string option Lwt.t)
      -> Rock.Request.t
      -> t option Lwt.t

    (** [user_from_session ?cookie_key ?secret ?key ?secret database_label request]
        returns the user that is associated to the user id in the session of the
        [request].

        [cookie_key] is the name/key of the session cookie. By default, the
        value is [_session].

        [secret] is used to verify the signature of the session cookie. By
        default, [SIHL_SECRET] is used.

        [key] is the key in the session associated with the user id. By default,
        the value is [user_id]. *)
    val user_from_session
      :  ?cookie_key:string
      -> ?secret:string
      -> ?key:string
      -> Database.Label.t
      -> Rock.Request.t
      -> t option Lwt.t
  end

  (** [search ?ctx ?sort ?filter ?limit ?offset ()] returns a list of users that
      is a partial view on all stored users.

      [sort] is the default sorting order of the created date. By default, this
      value is [`Desc].

      [filter] is a search keyword that is applied in a best-effort way on user
      details. The keyword has to occur in only one field (such as email).

      [limit] is the length of the returned list.

      [offset] is the pagination offset of the partial view. *)
  val search
    :  ?sort:[ `Desc | `Asc ]
    -> ?filter:string
    -> ?limit:int
    -> ?offset:int
    -> Database.Label.t
    -> unit
    -> (t list * int) Lwt.t

  (** [find_opt database_label id] returns a user with [id]. *)
  val find_opt : Database.Label.t -> string -> t option Lwt.t

  (** [find database_label id] returns a user with [id], [None] otherwise. *)
  val find : Database.Label.t -> string -> t Lwt.t

  (** [find_by_email database_label email] returns a [User.t] if there is a user with
      email address [email]. The lookup is case-insensitive. Raises an
      [{!Exception}] otherwise. *)
  val find_by_email : Database.Label.t -> string -> t Lwt.t

  (** [find_by_email_opt database_label email] returns a [User.t] if there is a user with
      email address [email]. *)
  val find_by_email_opt : Database.Label.t -> string -> t option Lwt.t

  (** [update_password ?password_policy database_label user ~old_password ~new_password
      ~new_password_confirmation]
      updates the password of a [user] to [new_password] and returns the user.
      The [old_password] is the current password that the user has to enter.
      [new_password] has to equal [new_password_confirmation].

      [password_policy] is a function that validates the [new_password] based on
      some password policy. By default, the policy is that a password has to be
      at least 8 characters long. *)
  val update_password
    :  ?password_policy:(string -> (unit, string) Result.t)
    -> Database.Label.t
    -> t
    -> old_password:string
    -> new_password:string
    -> new_password_confirmation:string
    -> (t, string) Result.t Lwt.t

  (** [update ?ctx?email ?username ?name ?given_name ?status user] stores the
      updated [user] and returns it. *)
  val update
    :  ?email:string
    -> ?username:string
    -> ?name:string
    -> ?given_name:string
    -> ?status:status
    -> Database.Label.t
    -> t
    -> t Lwt.t

  val update_details
    :  user:t
    -> email:string
    -> username:string option
    -> t Lwt.t
  [@@deprecated "Use update() instead"]

  (** [set_password ?ctx ?policy user ~password ~password_confirmation]
      overrides the current password of a [user] and returns that user.
      [password] has to equal [password_confirmation].

      [password_policy] is a function that validates the [new_password] based on
      some password policy. By default, the policy is that a password has to be
      at least 8 characters long.

      The current password doesn't have to be provided, therefore you should not
      expose this function to users but only admins. If you want the user to
      update their own password use {!update_password} instead. *)
  val set_password
    :  ?password_policy:(string -> (unit, string) Result.t)
    -> Database.Label.t
    -> t
    -> password:string
    -> password_confirmation:string
    -> (t, string) Result.t Lwt.t

  (** [create_user ?ctx ?id ?username ?name ?given_name email password] returns
      a non-admin user. Note that using [create_user] skips the registration
      workflow and should only be used with care.*)
  val create_user
    :  ?id:string
    -> ?username:string
    -> ?name:string
    -> ?given_name:string
    -> Database.Label.t
    -> password:string
    -> string
    -> t Lwt.t

  (** [create_admin ?ctx ?id ?username ?name ?given_name email password] returns
      an admin user. *)
  val create_admin
    :  ?id:string
    -> ?username:string
    -> ?name:string
    -> ?given_name:string
    -> Database.Label.t
    -> password:string
    -> string
    -> t Lwt.t

  (** [register_user ?ctx ?id ?password_policy ?username ?name ?given_name email password
      password_confirmation]
      creates a new user if the password is valid and if the email address was
      not already registered.

      Provide [password_policy] to check whether the password fulfills certain
      criteria. *)
  val register_user
    :  ?id:string
    -> ?password_policy:(string -> (unit, string) result)
    -> ?username:string
    -> ?name:string
    -> ?given_name:string
    -> Database.Label.t
    -> string
    -> password:string
    -> password_confirmation:string
    -> ( t
         , [ `Already_registered | `Invalid_password_provided of string ] )
         Result.t
         Lwt.t

  (** [login ?ctx email ~password] returns the user associated with [email] if
      [password] matches the current password. *)
  val login
    :  Database.Label.t
    -> string
    -> password:string
    -> (t, [ `Does_not_exist | `Incorrect_password ]) Result.t Lwt.t

  val register
    :  ?commands:Sihl.Command.t list
    -> unit
    -> Sihl.Container.Service.t

  include Sihl.Container.Service.Sig
end
