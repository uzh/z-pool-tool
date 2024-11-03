module Id : sig
  include Pool_model.Base.IdSig
end

type t =
  { id : Id.t
  ; target_contact_id : Contact.Id.t
  ; contact : Contact.t
  ; score : float
  }

val run : Database.Label.t -> Pool_common.Id.t -> unit Lwt.t
val find : Database.Label.t -> Id.t -> (t, Pool_message.Error.t) Lwt_result.t
val find_by_contact : Database.Label.t -> Contact.t -> t list Lwt.t
