type update_counters =
  { no_show : Assignment.NoShow.t
  ; participated : Assignment.Participated.t
  }

val update_on_invitation_sent : Contact.t -> Contact.t
val update_on_session_signup : Contact.t -> 'a list -> Contact.t
val update_on_assignment_from_waiting_list : Contact.t -> 'a list -> Contact.t

val update_on_session_closing
  :  Contact.t
  -> Assignment.NoShow.t
  -> Assignment.Participated.t
  -> Assignment.IncrementParticipationCount.t
  -> (Contact.t, Pool_message.Error.t) result

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
  -> current_values:update_counters
  -> updated_values:update_counters
  -> participated_in_other_assignments:bool
  -> Contact.t
