(* All events that are possible in the whole system *)

type t =
  | Admin of Admin.event
  | Announcement of Announcement.event
  | ApiKey of Api_key.event
  | Assignment of Assignment.event
  | AssignmentJob of Assignment_job.event
  | Contact of Contact.event
  | CustomField of Custom_field.event
  | Database of Pool_database.event
  | DuplicateContacts of Duplicate_contacts.event
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
  | PoolVersion of Pool_version.event
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
[@@deriving eq, show, variants]

let admin events = Admin events
let announcement events = Announcement events
let api_key events = ApiKey events
let assignment events = Assignment events
let assignmentjob events = AssignmentJob events
let contact events = Contact events
let custom_field events = CustomField events
let database events = Database events
let duplicate_contacts events = DuplicateContacts events
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
let pool_version events = PoolVersion events
let session events = Session events
let settings events = Settings events
let signupcode events = SignupCode events
let system_event events = SystemEvent events
let tags events = Tags events
let text_message events = TextMessage events
let time_window events = TimeWindow events
let user_import events = UserImport events
let user events = User events
let waiting_list events = WaitingList events

let handle_event ?(tags = Logs.Tag.empty) pool =
  let info model pp event =
    let tags = tags |> Database.Logger.Tags.add pool in
    let src = Logs.Src.create [%string "%{model}.events"] in
    Logs.info ~src (fun m -> m ~tags "Handle event %a" pp event)
  in
  function
  | Admin event ->
    info "admin" Admin.pp_event event;
    Admin.handle_event ~tags pool event
  | Announcement event ->
    info "announcement" Announcement.pp_event event;
    Announcement.handle_event pool event
  | ApiKey event ->
    info "api_key" Api_key.pp_event event;
    Api_key.handle_event ~tags pool event
  | Assignment event ->
    info "assignment" Assignment.pp_event event;
    Assignment.handle_event pool event
  | AssignmentJob event ->
    let src = Logs.Src.create "assignment.events" in
    Logs.info ~src (fun m ->
      m "Handle event %s" (Assignment_job.show_event event) ~tags);
    Assignment_job.handle_event pool event
  | Contact event ->
    info "contact" Contact.pp_event event;
    Contact.handle_event pool event
  | CustomField event ->
    info "custom_field" Custom_field.pp_event event;
    Custom_field.handle_event pool event
  | Database event ->
    info "database" Pool_database.pp_event event;
    Pool_database.handle_event pool event
  | DuplicateContacts event ->
    info "duplicate_contacts" Duplicate_contacts.pp_event event;
    Duplicate_contacts.handle_event pool event
  | Email event ->
    info "email" Email.pp_event event;
    Email.handle_event pool event
  | EmailVerification event ->
    info "email_verification" Email.pp_verification_event event;
    Email.handle_verification_event pool event
  | Experiment event ->
    info "experiment" Experiment.pp_event event;
    Experiment.handle_event pool event
  | Filter event ->
    info "Filter" Filter.pp_event event;
    Filter.handle_event pool event
  | Guard event ->
    info "guard" Guard.pp_event event;
    Guard.handle_event pool event
  | I18n event ->
    info "i18n" I18n.pp_event event;
    I18n.handle_event pool event
  | Invitation event ->
    info "invitation" Invitation.pp_event event;
    Invitation.handle_event pool event
  | Mailing event ->
    info "mailing" Mailing.pp_event event;
    Mailing.handle_event pool event
  | MessageTemplate event ->
    info "message_template" Message_template.pp_event event;
    Message_template.handle_event pool event
  | OrganisationalUnit event ->
    info "organisational_unit" Organisational_unit.pp_event event;
    Organisational_unit.handle_event pool event
  | PoolLocation event ->
    info "pool_location" Pool_location.pp_event event;
    Pool_location.handle_event pool event
  | PoolTenant event ->
    info "pool_tenant" Pool_tenant.pp_event event;
    Pool_tenant.handle_event pool event
  | PoolVersion event ->
    info "pool_version" Pool_version.pp_event event;
    Pool_version.handle_event event
  | Session event ->
    info "session" Session.pp_event event;
    Session.handle_event pool event
  | Settings event ->
    info "settings" Settings.pp_event event;
    Settings.handle_event pool event
  | SignupCode event ->
    info "signup_code" Signup_code.pp_event event;
    Signup_code.handle_event pool event
  | SystemEvent event ->
    info "system_event" System_event.pp_event event;
    (* Not passing pool, so the event can be handled with tenant events *)
    System_event.handle_event event
  | Tags event ->
    info "tags" Tags.pp_event event;
    Tags.handle_event pool event
  | TextMessage event ->
    info "text_message" Text_message.pp_event event;
    Text_message.handle_event pool event
  | TimeWindow event ->
    let src = Logs.Src.create "time_window.events" in
    Logs.info ~src (fun m ->
      m "Handle event %s" (Time_window.show_event event) ~tags);
    Time_window.handle_event pool event
  | UserImport event ->
    info "user_import" User_import.pp_event event;
    User_import.handle_event pool event
  | User event ->
    info "user" Pool_user.pp_event event;
    Pool_user.handle_event pool event
  | WaitingList event ->
    info "waiting_list" Waiting_list.pp_event event;
    Waiting_list.handle_event pool event
;;

let handle_events ?tags pool = Lwt_list.iter_s (handle_event ?tags pool)
