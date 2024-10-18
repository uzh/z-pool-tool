type t =
  | Action
  | Active
  | ActiveContactsCount
  | Actor
  | Admin
  | AdminComment
  | AdminHint
  | AdminInput
  | AdminInputOnly
  | AdminViewOnly
  | AllowUninvitedSignup
  | Announcement
  | Answer
  | AreaCode
  | Argument
  | AssetId
  | AssignableRole
  | Assignment
  | AssignmentCount
  | Assignments
  | AssignmentsCreated
  | AssignmentWithoutSession
  | Assistants
  | AvailableLanguages
  | Building
  | CallbackUrl
  | CanceledAt
  | CellPhone
  | Chronological
  | City
  | ClosedAt
  | Code
  | Confirmed
  | ConfirmedAt
  | Contact
  | ContactCount
  | ContactEmail
  | ContactLanguage
  | ContactPerson
  | Contacts
  | Context
  | CostCenter
  | Count
  | CreatedAt
  | CurrentPassword
  | CustomField
  | CustomFieldGroup
  | CustomFieldGroups
  | CustomFieldOption
  | CustomFieldOptions
  | CustomFields
  | CustomHtmx of (string * string)
  | Database
  | DatabaseLabel
  | DatabaseUrl
  | Date
  | DateTime
  | DefaultLanguage
  | DefaultSmtpServer
  | DeliveryReport
  | Description
  | DirectRegistrationDisabled
  | Disabled
  | Distribution
  | DistributionField
  | Duration
  | Email
  | EmailAddress
  | EmailAddressUnverified
  | EmailAddressVerified
  | EmailLeadTime
  | EmailLogo
  | EmailRemindersSentAt
  | EmailsSent
  | EmailSubject
  | EmailSuffix
  | EmailText
  | End
  | ExampleValue
  | Exclude
  | ExcludeRolesOf
  | Experiment
  | ExperimentCount
  | ExperimentEmailReminderLeadTime
  | Experimenter
  | Experiments
  | ExperimentTextMessageReminderLeadTime
  | ExperimentType
  | ExternalDataId
  | ExternalDataIdAbbr
  | ExternalDataRequired
  | Failed
  | FallbackToEmail
  | FieldType
  | File
  | FileMapping
  | FileMimeType
  | Filename
  | Filesize
  | Filter
  | Firstname
  | FirstReminder
  | FollowUpSession
  | GtxApiKey
  | GtxSender
  | HideCanceled
  | HideClosed
  | HideInactive
  | HideMakedAsDeleted
  | HidePast
  | HidePaused
  | HideUnverified
  | Hint
  | Host
  | I18n
  | Icon
  | Id
  | ImportPending
  | Inactive
  | InactiveUserDisableAfter
  | InactiveUserWarning
  | Input
  | Institution
  | InternalDescription
  | Interval
  | Invitation
  | InvitationCount
  | InvitationResetAt
  | Invitations
  | InvitationsSent
  | InvitationSubject
  | InvitationText
  | IsAdmin
  | Key
  | Label
  | Language
  | LanguageDe
  | LanguageEn
  | LastError
  | LastErrorAt
  | LastManuallyRemindedAt
  | Lastname
  | LastRemindedAt
  | LastRun
  | LastRunAt
  | LeadTime
  | Limit
  | Link
  | Location
  | Locations
  | LoginCount
  | LogoType
  | Mailing
  | MainSession
  | MarkedAsDeleted
  | MatchingFilterCount
  | MaxParticipants
  | MaxTries
  | Message
  | MessageChannel
  | MessageTemplate
  | MessageTemplates
  | MinParticipants
  | Model
  | Name
  | NewPassword
  | NextRunAt
  | NoShow
  | NoShowAbr
  | NoShowCount
  | NotifiedAt
  | NotifyContact
  | NotifyVia
  | NotMatchingFilterCount
  | Offset
  | OnlineExperiment
  | Operator
  | Operators
  | Order
  | OrganisationalUnit
  | Overbook
  | OverriddenValue
  | Override
  | Page
  | PageCount
  | Participant
  | ParticipantCount
  | Participants
  | Participated
  | ParticipatedAbr
  | ParticipationTag
  | ParticipationTags
  | PartnerLogos
  | Password
  | PasswordConfirmation
  | Paused
  | PendingContactImports
  | Period
  | Permission
  | PermissionOn of string * string
  | Placeholder
  | PlainText
  | Predicate
  | Profile
  | PromptOnRegistration
  | PublicDescription
  | PublicTitle
  | PublishedAt
  | Query
  | Queue
  | RandomOrder
  | Reason
  | Recipient
  | Redirect
  | Redirected
  | RegistrationDisabled
  | RegistrationPossible
  | Reminder
  | ReminderCount
  | RemindersSent
  | Required
  | ResentAt
  | Role
  | Room
  | Root
  | Rule
  | RunAt
  | ScheduledTime
  | ScheduledTimeSpan
  | Search
  | SearchOf of t
  | SecondReminder
  | Sender
  | SendingInvitations
  | SentAt
  | Session
  | SessionCount
  | Sessions
  | Setting
  | Settings
  | ShowExteralDataIdLinks
  | ShowUpCount
  | ShowToAdmins
  | ShowToContacts
  | SignedUpAt
  | SignUpCode
  | SignUpCount
  | SMS
  | SmsText
  | Smtp
  | SmtpLabel
  | SmtpMechanism
  | SmtpPassword
  | SmtpPort
  | SmtpProtocol
  | SmtpServer
  | SmtpUsername
  | SortOrder
  | Start
  | StartNow
  | Status
  | Street
  | Styles
  | Successful
  | Survey
  | SurveyUrl
  | SystemEvent
  | Tag
  | Tagging
  | Tags
  | Target
  | Template
  | Tenant
  | TenantDisabledFlag
  | TenantId
  | TenantLogos
  | TenantMaintenanceFlag
  | TenantPool
  | TermsAccepted
  | TermsAcceptedCount
  | TermsAndConditions
  | TermsAndConditionsLastAccepted
  | TestPhoneNumber
  | Text
  | TextMessage
  | TextMessageDlrStatus
  | TextMessageLeadTime
  | TextMessageRemindersSentAt
  | Time
  | TimeSpan
  | TimeUnit
  | TimeUnitOf of t
  | TimeWindow
  | Title
  | ToHandle
  | Token
  | Total
  | Translation
  | Tries
  | TriggerProfileUpdateAfter
  | UpdatedAt
  | Url
  | User
  | Validation
  | Value
  | ValueOf of t
  | VerificationCode
  | VerificationCount
  | Verified
  | Version
  | Virtual
  | WaitingList
  | Year
  | Zip

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val show : t -> string
val t_of_yojson : Yojson.Safe.t -> t
val yojson_of_t : t -> Yojson.Safe.t
val sexp_of_t : t -> Sexplib0.Sexp.t
val read_nested : string -> t option
val read : string -> t
val url_key : t -> string
val array_key : t -> string
val human_url : t -> string
val action : t
val activecontactscount : t
val actor : t
val admin : t
val admincomment : t
val adminhint : t
val admininput : t
val admininputonly : t
val adminviewonly : t
val allowuninvitedsignup : t
val answer : t
val areacode : t
val argument : t
val assetid : t
val assignablerole : t
val assignment : t
val assignmentcount : t
val assignments : t
val assignmentscreated : t
val assistants : t
val availablelanguages : t
val building : t
val canceledat : t
val cellphone : t
val chronological : t
val city : t
val closedat : t
val confirmedat : t
val contact : t
val contactcount : t
val contactemail : t
val contactlanguage : t
val contactperson : t
val contacts : t
val context : t
val costcenter : t
val count : t
val createdat : t
val currentpassword : t
val customfield : t
val customfieldgroup : t
val customfieldgroups : t
val customfieldoption : t
val customfieldoptions : t
val customfields : t
val customhtmx : string * string -> t
val database : t
val databaselabel : t
val databaseurl : t
val date : t
val datetime : t
val defaultlanguage : t
val defaultsmtpserver : t
val description : t
val directregistrationdisabled : t
val disabled : t
val distribution : t
val distributionfield : t
val duration : t
val email : t
val emailaddress : t
val emailaddressunverified : t
val emailaddressverified : t
val emailleadtime : t
val emailreminderssentat : t
val emailsubject : t
val emailsuffix : t
val emailtext : t
val emailssent : t
val end_ : t
val exclude : t
val excluderolesof : t
val experiment : t
val experimentcount : t
val experiments : t
val experimenter : t
val experimentemailreminderleadtime : t
val experimenttextmessagereminderleadtime : t
val experimenttype : t
val externaldataid : t
val externaldatarequired : t
val failed : t
val fieldtype : t
val file : t
val filemapping : t
val filemimetype : t
val filename : t
val filesize : t
val filter : t
val firstname : t
val firstreminder : t
val followupsession : t
val gtxapikey : t
val hidecanceled : t
val hideclosed : t
val hidemakedasdeleted : t
val hidepaused : t
val hidepast : t
val hideunverified : t
val hint : t
val host : t
val i18n : t
val icon : t
val id : t
val importpending : t
val inactiveuserdisableafter : t
val inactiveuserwarning : t
val input : t
val institution : t
val internaldescription : t
val interval : t
val invitation : t
val invitationcount : t
val invitationresetat : t
val invitations : t
val invitationsubject : t
val invitationssent : t
val invitationtext : t
val key : t
val label : t
val language : t
val languagede : t
val languageen : t
val lasterror : t
val lasterrorat : t
val lastname : t
val lastrun : t
val lastrunat : t
val leadtime : t
val limit : t
val link : t
val location : t
val locations : t
val logincount : t
val logotype : t
val mailing : t
val mainsession : t
val markedasdeleted : t
val maxparticipants : t
val maxtries : t
val message : t
val messagechannel : t
val messagetemplate : t
val messagetemplates : t
val minparticipants : t
val model : t
val name : t
val newpassword : t
val nextrunat : t
val noshow : t
val noshowabr : t
val noshowcount : t
val notifiedat : t
val notifyvia : t
val notifycontact : t
val offset : t
val operator : t
val operators : t
val order : t
val organisationalunit : t
val overbook : t
val overriddenvalue : t
val override : t
val page : t
val pagecount : t
val participant : t
val participantcount : t
val participants : t
val participated : t
val participatedabr : t
val participationtag : t
val partnerlogos : t
val password : t
val passwordconfirmation : t
val paused : t
val pendingcontactimports : t
val period : t
val permission : t
val plaintext : t
val predicate : t
val promptonregistration : t
val profile : t
val publicdescription : t
val publictitle : t
val publishedat : t
val query : t
val queue : t
val randomorder : t
val reason : t
val recipient : t
val redirect : t
val reminder : t
val registrationdisabled : t
val registrationpossible : t
val remindercount : t
val reminderssent : t
val lastmanuallyremindedat : t
val lastremindedat : t
val required : t
val resentat : t
val role : t
val room : t
val root : t
val rule : t
val runat : t
val scheduledtime : t
val scheduledtimespan : t
val search : t
val searchof : t -> t
val secondreminder : t
val sender : t
val sendinginvitations : t
val sentat : t
val sessioncount : t
val session : t
val sessions : t
val setting : t
val settings : t
val showupcount : t
val showexteraldataidlinks : t
val signedupat : t
val signupcount : t
val sms : t
val smstext : t
val smtp : t
val smtplabel : t
val smtpmechanism : t
val smtppassword : t
val smtpport : t
val smtpprotocol : t
val smtpserver : t
val smtpusername : t
val sortorder : t
val start : t
val startnow : t
val status : t
val street : t
val styles : t
val successful : t
val systemevent : t
val tag : t
val tags : t
val tagging : t
val target : t
val template : t
val tenant : t
val tenantdisabledflag : t
val tenantid : t
val tenantlogos : t
val tenantmaintenanceflag : t
val tenantpool : t
val termsaccepted : t
val termsacceptedcount : t
val termsandconditions : t
val testphonenumber : t
val textmessage : t
val textmessageleadtime : t
val textmessagereminderssentat : t
val time : t
val timespan : t
val timeunit : t
val timeunitof : t -> t
val title : t
val tohandle : t
val token : t
val total : t
val translation : t
val tries : t
val triggerprofileupdateafter : t
val url : t
val user : t
val validation : t
val value : t
val valueof : t -> t
val verified : t
val version : t
val virtual_ : t
val waitinglist : t
val year : t
val zip : t
