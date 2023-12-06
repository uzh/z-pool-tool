type experiment_type =
  | Lab
  | Online
[@@deriving eq, show]

type t [@@deriving eq, show]

val id : t -> Pool_common.Id.t
val title : t -> string
val public_title : t -> string
val description : t -> string option
val cost_center : t -> string option
val organisational_unit : t -> Organisational_unit.t option
val filter : t -> Filter.t option
val contact_person_id : t -> Pool_common.Id.t option
val smtp_auth_id : t -> Pool_common.Id.t option
val direct_registration_disabled : t -> bool
val registration_disabled : t -> bool
val allow_uninvited_signup : t -> bool
val external_data_required : t -> bool
val show_external_data_id_links : t -> bool
val experiment_type : t -> experiment_type
val email_session_reminder_lead_time : t -> Non_negative_time_span.t option
val invitation_reset_at : t -> Time_span.t option
val created_at : t -> Ptime.t
val updated_at : t -> Ptime.t

val text_message_session_reminder_lead_time
  :  t
  -> Non_negative_time_span.t option

(** Finds an experiment by id. *)
val find : Pool_database.Label.t -> Pool_common.Id.t -> t option Lwt.t

(** Inserts an experiment. *)
val insert : Pool_database.Label.t -> Model.t -> unit Lwt.t
