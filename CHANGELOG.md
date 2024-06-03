# Changelog

The format is based on [Keep a Changelog](http://keepachangelog.com/) and this project adheres to [Semantic Versioning](http://semver.org/).

## [unreleased](https://github.com/uzh/z-pool-tool/tree/HEAD)

## [0.7.5](https://github.com/uzh/z-pool-tool/tree/0.7.5) - 2024-06-03

### Added

- additional hints for text placeholders and its example values

### Changed

- `callbackUrl` for online experiments is now optional

### Fixed

- login with previously failed login attempt

## [0.7.4](https://github.com/uzh/z-pool-tool/tree/0.7.4) - 2024-05-16

### Fixed

- matcher does not consider online experiments to be fully booked

## [0.7.3](https://github.com/uzh/z-pool-tool/tree/0.7.3) - 2024-05-15

### Fixed

- validate email address process

## [0.7.2](https://github.com/uzh/z-pool-tool/tree/0.7.2) - 2024-05-15

### Added

- sessionless experiments with callback for participation (used for online experiments)
- location link to location overview text element

### Changed

- allow tenant databases to be unreachable and show info page
- increase minimum of interval for service status

### Fixed

- UI navigation elements for invitations
- continously check filter service registration
- login process with unanswered custom fields

## [0.7.1](https://github.com/uzh/z-pool-tool/tree/0.7.1) - 2024-04-30

### Fixed

- optional custom fields are now optional on registration as well
- assignment list print view fixed on Firefox and Safari
- only open a single buffer to write logs

## [0.7.0](https://github.com/uzh/z-pool-tool/tree/0.7.0) - 2024-04-11

### Added

- allow custom fields to be shown in the assignment list
- consistently check if assigned contacts still meet the criteria of the experiment
- notify admins when matcher does not find any more contacts
- UI to change role permissions

###  Changed

- restructured experiment page
- remove utility functions from guardian
- add a hint to filter template form
- change experiment contact person from admin to email address

### Fixed

- unique constraints for tenant database label
- only return distinct experiments on the dashboard
- allow assignment of contact by admin when registration is disabled
- create, update, and delete permissions for the experiment filter

## [0.6.3](https://github.com/uzh/z-pool-tool/tree/0.6.3) - 2024-03-19

### Added

- buttons to toggle all assignments on the session close screen
- functionality to directly contact one or more participants of a session

###  Changed

- include upcoming sessions in the participation list on the contact detail page
- include statistics about sent intiviations in experiment statistics

### Fixed

- experiment statistics shows correct session count. participation count and no-show count

## [0.6.2](https://github.com/uzh/z-pool-tool/tree/0.6.2) - 2024-03-07

### Added

- participation history to contact detail pages
- validation functionality for smtp settings

### Fixed

- vowel encoding issues in email subjects
- exception notifier separately adding labels
- ignore disabled contacts in statistics
- testing: user seed
- do not allow enrollment to waiting list when registration is disabled
- allow assignment of contact from waiting list when registration is disabled by an admin
- retain language when opening experiment detail page from contact dashboard

### Changes

- 'not found' error when contact accesses experiment without correct permissions
- removed upcomming sessions list on dashboard for recruiters and operators
- experiment search also includes tag titles

## [0.6.1](https://github.com/uzh/z-pool-tool/tree/0.6.1) - 2024-02-27

### Added

- admin dashboard: add incomplete and upcoming sessions
- switch session: refresh template when switching language for contact notification
- session duplication: create multiple at once (incl. one level of follow ups)
- experiment: statistics on detail page
- waiting list: button dropdown with profile link
- waiting list: allow sortable for signed up at
- report sql errors with canary
- invitation/migration emails: add opt out link (information to pause the account)

### Fixed

- calendar week view
- experiment detail view on mobile
- some of the translations
- Guardian access permissions for specific permissions

## [0.6.0](https://github.com/uzh/z-pool-tool/tree/0.6.0) - 2024-02-19

### Added

- Message History to experiment/contact
- Message History allow resend
- Exception notifier adds `Bug` and `Exception` labels
- Experimenter gets set if not general Experiment permission exist
- UI: define personal experiment create permission
- inform contact about cancellation
- session duration for follow ups within session confirmation email

### Fixed

- Staging email logo path
- Don't show sessions without parent session
- spelling
- show main session as long as the follow up isn't closed

### Changes

- use `Role<Role>` (e.g. `RoleAssistant`) target models to specify who can assign/unassign roles

## [0.5.5](https://github.com/uzh/z-pool-tool/tree/0.5.5) - 2024-02-12

### Added

- changable reminder interval for imported contacts/admins

### Changed

- sortable job queue UI

### Fixed

- make GTX key optional, show message if not set

## [0.5.4](https://github.com/uzh/z-pool-tool/tree/0.5.4) - 2024-01-24

### Added

- filter for assignments, sessions, experiments and mailings
- contact id as message temmplate text element

### Changed

- define minutes as default time unit
- prefill session data when duplicating or when creating followup session
- link participants to their profile page on assignment list
- move hardcoded role descriptions to translations

### Fixed

- Smtp auth cache differs between tenants
- order follow up sessions by start date

## [0.5.3](https://github.com/uzh/z-pool-tool/tree/0.5.3) - 2024-01-18

### Added

- modal to display current default message template for experiments and sessions

### Changed

- moved terms and conditions from settings page to texts page
- integrate search bar in sortable table
- allow time unit to be specified for all timespan inputs
- remove home title translation
- allow direct enrollment by admins to experiments with disabled registration
- split experiment and session description in public and internal

### Fixed

- waiting list displays enrolled contacts if assignment is marked as deleted

## [0.5.2](https://github.com/uzh/z-pool-tool/tree/0.5.2) - 2023-12-21

### Added

- option to enable min/max number of selected options for a multi_select custom field
- option to define an experiment language in which all messages are sent
- dev: reproducible package manager for OCaml (esy)
- support deleting smtp servers
- new filter fields for invitation and assignment
- sortable table for experiments overview page
- show custom field data on contact detail page
- mark unverified contact email addresses
- additional phone number validation
- status page to root api

### Fixed

- skip api key verification in non productive environments
- externalize form action for root gtx key update
- UI: info icon only on note message boxes
- show contact again in waiting list, when assignment is deleted
- email verification for admins

### Changed

- searchbar: remove sort and adjust UI
- filter: show contact count and already invited contacts separately
- don't show assigned contacts on waiting list
- move signup call to action to settings translations
- calendar view: show all sessions with link if user has permission
- allow to mark a multi select field as required (ability to define min/max amount of selections)
- use live search component for multi select fields
- contact overview page allow search for fullname
- allow manual session assignment for past events
- upgrade packages

## [0.5.1](https://github.com/uzh/z-pool-tool/tree/0.5.1) - 2023-11-08

### Fixed

- modals e.g. terms and conditions, resend reminder
- mailing overlaps view
- mailing create form
- clear cache when adding an admin
- permission to update waiting list element

## [0.5.0](https://github.com/uzh/z-pool-tool/tree/0.5.0) - 2023-11-06

### Added

- many hints (e.g. default lead time, contact filter, message templates, experiment roles)
- reschedule session confirmable
- mailings: display the amount of invitations handeld within the mailing
- setup: add short commit SHA to non-production builds
- logging: add IP address

### Fixed

- settings: show role permissions to recruiters and operators
- upcoming session overview for contacts
- do not notify uncanceled assignemtns when rescheduling
- custom fields: group button naming
- root middleware for login
- mobile session UI

### Changed

- standardize the creation of search components
- custom fields: use multi select in filter form for select custom fields
- custom fields: do not allow publication of select / multi-select custom fields without options
- mailing: uses now a limit (N emails during the mailing period) instead of rate (hourly rate)
- mailing: check random order by default
- mailing: hide subform when random is checked
- settings: restructure email suffix form
- settings: change disable_profile_after from weeks to days
- settings: rename rules to role permission

## [0.4.8](https://github.com/uzh/z-pool-tool/tree/0.4.8) - 2023-10-24

### Added

- notification when a contact changes their email address to an existing one
- layout for session print view
- session enrollment through contact detail page
- autofill experiment public title
- ability to reset the invitations per experiment
- optional mapping between mailings and invitations

### Fixed

- updating contact email address will not update email if the address is already in use
- order of delete requests
- use mrmime package to validate email address
- only the specified tag in search gets handled (not all search results)

### Changed

- required default data is seeded in the migrations
- consider user language in password reset email
- queue reports issues only when it finally fails
- logging of newly registered users
- resend invitation UI adjustments

## [0.4.7](https://github.com/uzh/z-pool-tool/tree/0.4.7) - 2023-09-27

### Added

- display session and assignment reminder sent timestamps in resend single reminder modal
- waiting list signup confirmation email
- page to list all external data ids of a contact

### Changed

- performance update for guardian, rework roles
- functionality to change the session of an assignment

## [0.4.6](https://github.com/uzh/z-pool-tool/tree/0.4.6) - 2023-09-18

### Added

- icons for paused / verified status of contacts
- send reminders via text message
- location description per language
- resend a single reminder

### Fixed

- Layout of navigation in footer on mobile devices
- Retain filter when using pagination
- checking if contact matches filter
- location address format
- filter for including tags
- session registration sessions with cancelled assignments
- searchable created at field for waiting list
- text message intercepter

### Changed

- improve performance of 'contains none' and 'contains some' filters
- refactored session close view layout
- placed buttons of session list in a dropdown
- improve location create form
- sort locations alphabetically
- hide fully booked sessions for contact registration
- always show hint about empty session list for contact registration
- reformat displayed session start/end/duration
- add message template element for session start/end/duration

## [0.4.5](https://github.com/uzh/z-pool-tool/tree/0.4.5) - 2023-08-22

### Added

- added edit form assignments of closed sessions
- form to resend reminders manually

### Changed

- session close screen updates assignments on change

### Fixed

- filtering by multiple experiment participations
- input naming conflict in search form
- location path in message templates

## [0.4.4](https://github.com/uzh/z-pool-tool/tree/0.4.4) - 2023-08-10

### Added

- option to delete experiment and session specific message templates

### Changes

- rename allow all contacts to register for an experiment
- extended available text elements in email templates
- sort assignments alphabetically
- sort locations alphabetically
- show zurich timestamp in logs

### Fixed

- linebreaks in html emails

## [0.4.3](https://github.com/uzh/z-pool-tool/tree/0.4.3) - 2023-08-09

### Changes

- reminder runs every minute
- matcher runs every five minutes
- update naming for number of assignments

### Fixed

- persist checkboxes of session close view
- validation of mailing rate in matcher

## [0.4.2](https://github.com/uzh/z-pool-tool/tree/0.4.2) - 2023-08-08

### Fixed

- keep filter when updating experiment

## [0.4.1](https://github.com/uzh/z-pool-tool/tree/0.4.1) - 2023-08-08

### Added

- external_data_required to experiments
- external_data_id to assignments (required latest on session close)

### Changed

- include contacts whose import is pending in filter
- allow password reset when import is pending
- hide contact information when not allowed to read it
- split first-/lastname of contact information list in session assignments

## [0.4.0](https://github.com/uzh/z-pool-tool/tree/0.4.0) - 2023-08-03

### Added

- readonly admin comment on contacts
- credits and privacy policy pages
- basic admin statistics
- ability to promote a contact to an admin

### Changed

- log failed login attempt as warning

### Fixed

- blocking of email addresses after too many failed login attempts

## [0.3.3](https://github.com/uzh/z-pool-tool/tree/0.3.3) - 2023-08-02

### Added

- canary notifier for job worker

## [0.3.2](https://github.com/uzh/z-pool-tool/tree/0.3.2) - 2023-07-31

### Added

- option to prompt custom fields when signing up

### Changed

- ignore boolean custom fields when checking profile completeness

## [0.3.1](https://github.com/uzh/z-pool-tool/tree/0.3.1) - 2023-07-28

### Added

- link to session and experiment details in calender popup
- remove paused from partial update and create separate handlers

### Changed

- allow sending invitations to contacts without terms and conditions accepted
- many small UI/UX adjustments

## [0.3.0](https://github.com/uzh/z-pool-tool/tree/0.3.0) - 2023-07-26

### Added

- text message service
- option to send session cancellation messages as text messages
- validation of gtx api keys when adding or updating tenants
- notification and confirmation service for user imports
- allow smtp account and reply-to address to be specified for each experiment
- calendar that shows all sessions of a location and all sessions of the current admin user
- tags for experiments and contacts (admin sites)
- colorize calendar
- email plain text hint
- tag predicate for filters, allows to filter contact tags
- option to define experiment and session specific tags that are assigned to all participants

### Changed

- development environment
- CLI commands
- remove session list from location detail
- time span picker using minutes
- only validate experiment when loading sessions for calendar

### Fixed

- added failed_login_attempts seed to root DB
- only show future events on contact dashboard to sign up
- allow contacts to access experiment when assignment exists
- initialize flatpickr correctly after htmx swap

## [0.2.1](https://github.com/uzh/z-pool-tool/tree/0.2.1) - 2023-06-01

### Fixed

- detailed view of tenants
- users in the root overview
- common root routes which are targeting denied/not-found via redirects
- missing delete functionality for rules

### Added

- profile pages for admins in root and tenants

## [0.2.0](https://github.com/uzh/z-pool-tool/tree/0.2.0) - 2023-05-26

### Added

- reply-to address to emails, using tenant contact email address (system settings)
- block email addresses after multiple failed login attempts
- buttons to reset forms
- validation for tenant database urls, when creating or updating a tenant
- role based navigation bar
- cached version of guardian validation
- admin roles to the detail page and grant/revoke role to edit page
- added session expiration
- rules page in settings
- filter by no-show count
- filter by empty / non-empty fields
- WYSIWYG editor for location description
- allow mailing to start now with checkbox

### Changed

- added stronger password policy
- tenant description, icon and styles are optional
- refactor and update guardian integration and middleware
- using uuids instead of ids as foreign keys in invitations and mailings tables
- using uuids instead of ids as foreign keys in assignment table
- when signing up for a session with follow-ups, the contact receives one confirmation message containing all sessions
- allow contacts to resignup after session cancellation
- updated session close view and assignments to store no-shows instead of show-ups
- switch dependency between assignment and session
- allow assignments to be marked as deleted
- migrate search endpoints (experiment/location)
- improved cookie settings
- improve experiment waiting list for already assigned contacts
- contact specific counters for number of invitations, show-ups, no-shows and participated experiments
- allow reminders to be sent more than 24h in advance
- improve error log and exceptions

### Fixed

- filter by boolean fields
- custom fields with the option 'admin_view_only' will automatically have 'admin_input_only' set to true
- partial update of 'admin_view_only' fields
- when cancelling an assignment, assignments of the same contact to follow-up sessions will be canceled as well
- canceled assignments are not included in assignment count anymore
- canceled assignments are no longer shown on the contact dashboard
- do not include canceled assignments in session assignment_count
- when cancelling a session, follow-up sessions to the session are canceled as well
- use specific tenant settings command effects
- access middleware for mailing search info and system settings
- wrong location on session duplication
- several small UI adjustments

## [0.1.3](https://github.com/uzh/z-pool-tool/tree/0.1.3) - 2023-03-01

### Added

- pagination, search and sort functionality for lists

### Fixed

- queue worker service context parsing

## [0.1.2](https://github.com/uzh/z-pool-tool/tree/0.1.2) - 2023-02-27

### Added

- add description to user profile completion page
- allow sessions to be closed before they end
- make pool_assignments `show_up` and `participated` nullable
- detect unsaved changes in forms
- send notification to contacts when there is an registration attempt with in-use email address
- allow contacts to be filtered by experiment participation
- UI displaying schedules (label, interval, status, last_run)
- UI displaying jobs in queue

### Changed

- split server command and queue worker command
- move smtp configurations from root to tenants
- integrate email service in tenant
- show add configuration when not configured already
- extract worker scheduler
- improve lwt ignore handling

### Fixed

- removed default value from pool_sessions.start
- add language parameter to email links
- improve consistency (schedule, queue worker, matcher)

### Changed

- message_template does not fall back to global template, if a language of entity specific templates is missing.

## [0.1.1](https://github.com/uzh/z-pool-tool/tree/0.1.1) - 2023-02-02

### Added

- add wysiwyg editor
- add plain text email
- hide overriden admin values from contacts
- only show user value when admin_override is disabled
- display a hint or answers of the contact when overridden
- allow non required fields to be cleared

## [0.1.0](https://github.com/uzh/z-pool-tool/tree/0.1.0) - 2023-01-23

### Added

- handling of entity based email templates (e.g. Experiment Invitation, Experiment - Session Reminder, Session - Session Reminder)
- detailed email template form description, show available text elements
- version to assets
- participant dashboard
- redirect to intended page when login is needed
- keep email address on failed login page
- encrypting root database information

### Fixed

- development seed - don't generate duplicate persons
- empty filter
- deleting an experiment removes it's filter as well

## [0.0.4](https://github.com/uzh/z-pool-tool/tree/0.0.4) - 2023-01-13

### Added

- pause sending invitations when no spots in sessions are available and add note on create mailing page

### Removed

- Recruitment channel from contact module (add as custom field)

### Fixed

- don't logout contacts when answering required fields
- experiments with active uninvited registration handle the experiment filter
- effects for waiting list session registration (admin)
- signup for follow up sessions is handled within the main session signup
- consistent email naming
- mailing schema handle correct default value

## [0.0.3](https://github.com/uzh/z-pool-tool/tree/0.0.3) - 2022-12-23

### Removed

- unused person table

### Fixed

- display version in footer
- verification note typo
- deployment process

## [0.0.2](https://github.com/uzh/z-pool-tool/tree/0.0.2) - 2022-12-22

### Added

- option to make the matcher select the contacts in random order
- display version in footer
- CLI: create admin, grant roles and list roles

### Fixed

- create admin via tenant UI
- contact count for invitations

## [0.0.1](https://github.com/uzh/z-pool-tool/tree/0.0.1) - 2022-12-16

💥 The start of using the changelog. Preparations for our pilot version 0.1.0.

### Fixed

- create operator via root UI

### Changed

- use versions without `v` prefix as we do in all our other projects
