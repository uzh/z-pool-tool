type t =
  { target_user_uuid : Pool_common.Id.t
  ; user_uuid : Pool_common.Id.t
  ; email : Pool_user.EmailAddress.t
  ; similarity_score : float
  }

val run : Database.Label.t -> Pool_common.Id.t -> unit Lwt.t
val find_by_contact : Database.Label.t -> Pool_common.Id.t -> t list Lwt.t
