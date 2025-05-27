(** [create ?expires_in ?secret database_label data] returns a token that expires in
    [expires_in] with the associated data [data]. If no [expires_in] is set,
    the default is 7 days. An optional secret [secret] can be provided for the
    token signature, by default `SIHL_SECRET` is used. *)
val create
  :  ?secret:string
  -> ?expires_in:Sihl.Time.duration
  -> Database.Label.t
  -> (string * string) list
  -> string Lwt.t

(** [read ?secret ?force database_label token k] returns the value that is associated
    with the key [k] in the token [token]. If [force] is set, the value is
    read and returned even if the token is expired, deactivated and the
    signature is invalid. If the token is completely invalid and can not be
    read, no value is returned. An optional secret [secret] can be provided to
    override the default `SIHL_SECRET`. *)
val read
  :  ?secret:string
  -> ?force:unit
  -> Database.Label.t
  -> string
  -> k:string
  -> string option Lwt.t

(** [read_all ?secret ?force database_label token] returns all key-value pairs
    associated with the token [token]. If [force] is set, the values are read
    and returned even if the token is expired, deactivated and the signature
    is invalid. If the token is completely invalid and can not be read, no
    value is returned. An optional secret [secret] can be provided to override
    the default `SIHL_SECRET`.*)
val read_all
  :  ?secret:string
  -> ?force:unit
  -> Database.Label.t
  -> string
  -> (string * string) list option Lwt.t

(** [verify ?secret database_label token] returns true if the token has a valid
    structure and the signature is valid, false otherwise. An optional secret
    [secret] can be provided to override the default `SIHL_SECRET`. *)
val verify : ?secret:string -> Database.Label.t -> string -> bool Lwt.t

(** [deactivate database_label token] deactivates the token. Depending on the backend of
    the token service a blacklist is used to store the token. *)
val deactivate : Database.Label.t -> string -> unit Lwt.t

(** [activate database_label token] re-activates the token. Depending on the backend of
    the token service a blacklist is used to store the token. *)
val activate : Database.Label.t -> string -> unit Lwt.t

(** [is_active database_label token] returns true if the token is active, false if the
    token was deactivated. An expired token or a token that has an invalid
    signature is not necessarily inactive.*)
val is_active : Database.Label.t -> string -> bool Lwt.t

(** [is_expired database_label token] returns true if the token is expired, false
    otherwise. An optional secret [secret] can be provided to override the
    default `SIHL_SECRET`. *)
val is_expired : ?secret:string -> Database.Label.t -> string -> bool Lwt.t

(** [is_valid database_label token] returns true if the token is not expired, active and
    the signature is valid and false otherwise. A valid token can safely be
    used. An optional secret [secret] can be provided to override the default
    `SIHL_SECRET`. *)
val is_valid : ?secret:string -> Database.Label.t -> string -> bool Lwt.t

val find_active_by_data
  :  Database.Label.t
  -> (string * string) list
  -> string option Lwt.t

val lifecycle : Sihl.Container.lifecycle
val register : unit -> Sihl.Container.Service.t
