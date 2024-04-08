(* All events that are possible in the whole system *)

type t =
  | Admin of Admin.event
  | Assignment of Assignment.event
  | AssignmentJob of Assignment_job.event
  | Contact of Contact.event
  | CustomField of Custom_field.event
  | Database of Database.event
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
  | SystemEvent of System_event.event
  | Tags of Tags.event
  | TextMessage of Text_message.event
  | UserImport of User_import.event
  | WaitingList of Waiting_list.event
[@@deriving eq, show, variants]

let admin events = Admin events
let assignment events = Assignment events
let assignmentjob events = AssignmentJob events
let contact events = Contact events
let custom_field events = CustomField events
let database events = Database events
let email events = Email events
let email_verification events = EmailVerification events
let experiment events = Experiment events
let filter events = Filter events
let guard events = Guard events
let i18n events = I18n events
let invitation events = Invitation events
let mailing events = Mailing events
let message_template events = MessageTemplate events
let organisational_unit events = OrganisationalUnit events
let pool_location events = PoolLocation events
let pool_tenant events = PoolTenant events
let session events = Session events
let settings events = Settings events
let system_event events = SystemEvent events
let tags events = Tags events
let text_message events = TextMessage events
let user_import events = UserImport events
let waiting_list events = WaitingList events

let handle_event ?(tags = Logs.Tag.empty) pool event =
  let tags = tags |> Pool_database.Logger.Tags.add pool in
  match event with
  | Admin event ->
    let src = Logs.Src.create "admin.events" in
    Logs.info ~src (fun m -> m "Handle event %s" (Admin.show_event event) ~tags);
    Admin.handle_event ~tags pool event
  | Assignment event ->
    let src = Logs.Src.create "assignment.events" in
    Logs.info ~src (fun m ->
      m "Handle event %s" (Assignment.show_event event) ~tags);
    Assignment.handle_event pool event
  | AssignmentJob event ->
    let src = Logs.Src.create "assignment.events" in
    Logs.info ~src (fun m ->
      m "Handle event %s" (Assignment_job.show_event event) ~tags);
    Assignment_job.handle_event pool event
  | Contact event ->
    let src = Logs.Src.create "contact.events" in
    Logs.info ~src (fun m ->
      m "Handle event %s" (Contact.show_event event) ~tags);
    Contact.handle_event pool event
  | CustomField event ->
    let src = Logs.Src.create "custom_field.events" in
    Logs.info ~src (fun m ->
      m "Handle event %s" (Custom_field.show_event event) ~tags);
    Custom_field.handle_event pool event
  | Database event ->
    let src = Logs.Src.create "database.events" in
    Logs.info ~src (fun m ->
      m "Handle event %s" (Database.show_event event) ~tags);
    Database.handle_event pool event
  | Email event ->
    let src = Logs.Src.create "email.events" in
    Logs.info ~src (fun m -> m "Handle event %s" (Email.show_event event) ~tags);
    Email.handle_event pool event
  | EmailVerification event ->
    let src = Logs.Src.create "email_verification.events" in
    Logs.info ~src (fun m ->
      m "Handle event %s" (Email.verification_event_name event) ~tags);
    Email.handle_verification_event pool event
  | Experiment event ->
    let src = Logs.Src.create "experiment.events" in
    Logs.info ~src (fun m ->
      m "Handle event %s" (Experiment.show_event event) ~tags);
    Experiment.handle_event pool event
  | Filter event ->
    let src = Logs.Src.create "filter.events" in
    Logs.info ~src (fun m ->
      m "Handle event %s" (Filter.show_event event) ~tags);
    Filter.handle_event pool event
  | Guard event ->
    let src = Logs.Src.create "guard.events" in
    Logs.info ~src (fun m -> m "Handle event %s" (Guard.show_event event) ~tags);
    Guard.handle_event pool event
  | I18n event ->
    let src = Logs.Src.create "i18n.events" in
    Logs.info ~src (fun m -> m "Handle event %s" (I18n.show_event event) ~tags);
    I18n.handle_event pool event
  | Invitation event ->
    let src = Logs.Src.create "invitation.events" in
    Logs.info ~src (fun m ->
      m "Handle event %s" (Invitation.show_event event) ~tags);
    Invitation.handle_event pool event
  | Mailing event ->
    let src = Logs.Src.create "mailing.events" in
    Logs.info ~src (fun m ->
      (* TODO [josef] use event name *)
      m "Handle event %s" (Mailing.show_event event) ~tags);
    Mailing.handle_event pool event
  | MessageTemplate event ->
    let src = Logs.Src.create "mailing.events" in
    Logs.info ~src (fun m ->
      (* TODO [josef] use event name *)
      m "Handle event %s" (Message_template.show_event event) ~tags);
    Message_template.handle_event pool event
  | OrganisationalUnit event ->
    let src = Logs.Src.create "organisational_unit.events" in
    Logs.info ~src (fun m ->
      m "Handle event %s" (Organisational_unit.show_event event) ~tags);
    Organisational_unit.handle_event pool event
  | PoolLocation event ->
    let src = Logs.Src.create "pool_location.events" in
    Logs.info ~src (fun m ->
      m "Handle event %s" (Pool_location.show_event event) ~tags);
    Pool_location.handle_event pool event
  | PoolTenant event ->
    let src = Logs.Src.create "pool_tenant.events" in
    Logs.info ~src (fun m ->
      m "Handle event %s" (Pool_tenant.show_event event) ~tags);
    Pool_tenant.handle_event pool event
  | Session event ->
    let src = Logs.Src.create "session.events" in
    Logs.info ~src (fun m ->
      m "Handle event %s" (Session.show_event event) ~tags);
    Session.handle_event pool event
  | Settings event ->
    let src = Logs.Src.create "settings.events" in
    Logs.info ~src (fun m ->
      m "Handle event %s" (Settings.show_event event) ~tags);
    Settings.handle_event pool event
  | SystemEvent event ->
    let src = Logs.Src.create "system_event.events" in
    Logs.info ~src (fun m ->
      m "Handle event %s" (System_event.show_event event) ~tags);
    (* Not passing pool, so the event can be handled with tenant events *)
    System_event.handle_event event
  | Tags event ->
    let src = Logs.Src.create "tags.events" in
    Logs.info ~src (fun m -> m "Handle event %s" (Tags.show_event event) ~tags);
    Tags.handle_event pool event
  | TextMessage event ->
    let src = Logs.Src.create "text_message.events" in
    Logs.info ~src (fun m ->
      m "Handle event %s" (Text_message.show_event event) ~tags);
    Text_message.handle_event pool event
  | UserImport event ->
    let src = Logs.Src.create "user_import.events" in
    Logs.info ~src (fun m ->
      m "Handle event %s" (User_import.show_event event) ~tags);
    User_import.handle_event pool event
  | WaitingList event ->
    let src = Logs.Src.create "waiting_list.events" in
    Logs.info ~src (fun m ->
      m "Handle event %s" (Waiting_list.show_event event) ~tags);
    Waiting_list.handle_event pool event
;;

let handle_events ?tags pool = Lwt_list.iter_s (handle_event ?tags pool)
