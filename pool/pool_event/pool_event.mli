type t =
  | Admin of Admin.event
  | Announcement of Announcement.event
  | Assignment of Assignment.event
  | AssignmentJob of Assignment_job.event
  | Contact of Contact.event
  | CustomField of Custom_field.event
  | Database of Pool_database.event
  | Email of Email.event
  | EmailVerification of Email.verification_event
  | Experiment of Experiment.event
  | Filter of Filter.event
  | Guard of Guard.event
  | I18n of I18n.event
  | Invitation of Invitation.event
  | Mailing of Mailing.event
  | MessageTemplate of Message_template.event
  | OrganisationalUnit of Organisational_unit.event
  | PoolLocation of Pool_location.event
  | PoolTenant of Pool_tenant.event
  | Session of Session.event
  | Settings of Settings.event
  | SignupCode of Signup_code.event
  | SystemEvent of System_event.event
  | Tags of Tags.event
  | TextMessage of Text_message.event
  | TimeWindow of Time_window.event
  | UserImport of User_import.event
  | User of Pool_user.event
  | WaitingList of Waiting_list.event

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val show : t -> string
val admin : Admin.event -> t
val announcement : Announcement.event -> t
val assignment : Assignment.event -> t
val assignmentjob : Assignment_job.event -> t
val contact : Contact.event -> t
val custom_field : Custom_field.event -> t
val database : Pool_database.event -> t
val email : Email.event -> t
val email_verification : Email.verification_event -> t
val experiment : Experiment.event -> t
val filter : Filter.event -> t
val guard : Guard.event -> t
val i18n : I18n.event -> t
val invitation : Invitation.event -> t
val mailing : Mailing.event -> t
val message_template : Message_template.event -> t
val organisational_unit : Organisational_unit.event -> t
val pool_location : Pool_location.event -> t
val pool_tenant : Pool_tenant.event -> t
val session : Session.event -> t
val settings : Settings.event -> t
val signupcode : Signup_code.event -> t
val system_event : System_event.event -> t
val tags : Tags.event -> t
val text_message : Text_message.event -> t
val time_window : Time_window.event -> t
val user_import : User_import.event -> t
val user : Pool_user.event -> t
val waiting_list : Waiting_list.event -> t

val handle_event
  :  ?tags:Logs.Tag.set
  -> Database.Label.t
  -> Pool_context.user
  -> t
  -> unit Lwt.t

val handle_events
  :  ?tags:Logs.Tag.set
  -> Database.Label.t
  -> Pool_context.user
  -> t list
  -> unit Lwt.t

val handle_system_event
  :  ?tags:Logs.Tag.set
  -> Database.Label.t
  -> t
  -> unit Lwt.t

val handle_system_events
  :  ?tags:Logs.Tag.set
  -> Database.Label.t
  -> t list
  -> unit Lwt.t
