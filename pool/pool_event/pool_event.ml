(* All events that are possible in the whole system *)
type t =
  | Admin of Admin.event
  | Database of Database.event
  | EmailAddress of Email.event
  | Experiment of Experiment.event
  | I18n of I18n.event
  | Participant of Participant.event
  | Participation of Participation.event
  | PoolTenant of Pool_tenant.event
  | Root of Root.event
  | Settings of Settings.event
  | Tenant of Tenant.event
[@@deriving eq, show]

let admin events = Admin events
let database events = Database events
let email_address events = EmailAddress events
let experiment events = Experiment events
let i18n events = I18n events
let participant events = Participant events
let participation events = Participation events
let pool_tenant events = PoolTenant events
let root events = Root events
let settings events = Settings events
let tenant events = Tenant events

let handle_event pool event =
  match event with
  | Admin event -> Admin.handle_event pool event
  | Database event -> Database.handle_event pool event
  | EmailAddress event -> Email.handle_event pool event
  | Experiment event -> Experiment.handle_event pool event
  | I18n event -> I18n.handle_event pool event
  | Participant event -> Participant.handle_event pool event
  | Participation event -> Participation.handle_event pool event
  | PoolTenant event -> Pool_tenant.handle_event pool event
  | Root event -> Root.handle_event pool event
  | Settings event -> Settings.handle_event pool event
  | Tenant event -> Tenant.handle_event pool event
;;

let handle_events pool = Lwt_list.iter_s (handle_event pool)
