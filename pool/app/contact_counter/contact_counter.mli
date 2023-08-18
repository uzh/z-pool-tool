val update_on_invitation_sent : Contact.t -> Contact.t
val update_on_session_signup : Contact.t -> 'a list -> Contact.t
val update_on_assignment_from_waiting_list : Contact.t -> 'a list -> Contact.t

val update_on_session_closing
  :  Contact.t
  -> Assignment.NoShow.t
  -> Assignment.Participated.t
  -> Assignment.IncrementParticipationCount.t
  -> (Contact.t, Pool_common.Message.error) result

val update_on_session_cancellation : Assignment.t list -> Contact.t -> Contact.t

val update_on_assignment_cancellation
  :  Assignment.t list
  -> Contact.t
  -> Contact.t

val update_on_assignment_deletion
  :  Assignment.t list
  -> Contact.t
  -> Assignment.IncrementParticipationCount.t
  -> Contact.t

val update_on_assignment_update
  :  Assignment.t
  -> Assignment.NoShow.t
  -> Assignment.NoShow.t
  -> bool
  -> Contact.t
