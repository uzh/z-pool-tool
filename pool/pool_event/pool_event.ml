(* All events that are possible in the whole system *)
type t =
  | Admin of Admin.event
  | Database of Database.event
  | EmailAddress of Email.event
  | Experiment of Experiment.event
  | I18n of I18n.event
  | Invitation of Invitation.event
  | Participation of Participation.event
  | PoolTenant of Pool_tenant.event
  | Root of Root.event
  | Session of Session.event
  | Settings of Settings.event
  | Subject of Subject.event
  | Tenant of Tenant.event
  | WaitingList of Waiting_list.event
[@@deriving eq, show]

let admin events = Admin events
let database events = Database events
let email_address events = EmailAddress events
let experiment events = Experiment events
let i18n events = I18n events
let invitation events = Invitation events
let participation events = Participation events
let pool_tenant events = PoolTenant events
let root events = Root events
let session events = Session events
let settings events = Settings events
let subject events = Subject events
let tenant events = Tenant events
let waiting_list events = WaitingList events

let handle_event pool event =
  match event with
  | Admin event -> Admin.handle_event pool event
  | Database event -> Database.handle_event pool event
  | EmailAddress event -> Email.handle_event pool event
  | Experiment event -> Experiment.handle_event pool event
  | I18n event -> I18n.handle_event pool event
  | Invitation event -> Invitation.handle_event pool event
  | Participation event -> Participation.handle_event pool event
  | PoolTenant event -> Pool_tenant.handle_event pool event
  | Root event -> Root.handle_event pool event
  | Session event -> Session.handle_event pool event
  | Settings event -> Settings.handle_event pool event
  | Subject event -> Subject.handle_event pool event
  | Tenant event -> Tenant.handle_event pool event
  | WaitingList event -> Waiting_list.handle_event pool event
;;

let handle_events pool = Lwt_list.iter_s (handle_event pool)
