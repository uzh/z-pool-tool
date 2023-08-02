module ActiveContacts : sig
  include Pool_common.Model.IntegerSig
end

module PendingContactImports : sig
  include Pool_common.Model.IntegerSig
end

module AssignmentsCreated : sig
  include Pool_common.Model.IntegerSig
end

module InvitationsSent : sig
  include Pool_common.Model.IntegerSig
end

module SignUpCount : sig
  include Pool_common.Model.IntegerSig
end

type period =
  | Min15
  | Hour1
  | Day1
  | Month1

val default_period : period

type t =
  { active_contacts : ActiveContacts.t
  ; pending_contact_imports : PendingContactImports.t
  ; assignments_created : AssignmentsCreated.t
  ; invitations_sent : InvitationsSent.t
  ; sign_up_count : SignUpCount.t
  }

val create : Pool_database.Label.t -> ?period:period -> unit -> t Lwt.t
val yojson_of_t : t -> Yojson.Safe.t
