# Changelog

The format is based on [Keep a Changelog](http://keepachangelog.com/) and this project adheres to [Semantic Versioning](http://semver.org/).

## [unreleased](https://github.com/uzh/pool/tree/HEAD)

## [0.4.6](https://github.com/uzh/pool/tree/0.4.6) - 2023-09-18

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

## [0.4.5](https://github.com/uzh/pool/tree/0.4.5) - 2023-08-22

### Added

- added edit form assignments of closed sessions
- form to resend reminders manually

### Changed

- session close screen updates assignments on change

### Fixed

- filtering by multiple experiment participations
- input naming conflict in search form
- location path in message templates

## [0.4.4](https://github.com/uzh/pool/tree/0.4.4) - 2023-08-10

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

## [0.4.3](https://github.com/uzh/pool/tree/0.4.3) - 2023-08-09

### Changes

- reminder runs every minute
- matcher runs every five minutes
- update naming for number of assignments

### Fixed

- persist checkboxes of session close view
- validation of mailing rate in matcher

## [0.4.2](https://github.com/uzh/pool/tree/0.4.2) - 2023-08-08

### Fixed

- keep filter when updating experiment

## [0.4.1](https://github.com/uzh/pool/tree/0.4.1) - 2023-08-08

### Added

- external_data_required to experiments
- external_data_id to assignments (required latest on session close)

### Changed

- include contacts whose import is pending in filter
- allow password reset when import is pending
- hide contact information when not allowed to read it
- split first-/lastname of contact information list in session assignments

## [0.4.0](https://github.com/uzh/pool/tree/0.4.0) - 2023-08-03

### Added

- readonly admin comment on contacts
- credits and privacy policy pages
- basic admin statistics
- ability to promote a contact to an admin

### Changed

- log failed login attempt as warning

### Fixed

- blocking of email addresses after too many failed login attempts

## [0.3.3](https://github.com/uzh/pool/tree/0.3.3) - 2023-08-02

### Added

- canary notifier for job worker

## [0.3.2](https://github.com/uzh/pool/tree/0.3.2) - 2023-07-31

### Added

- option to prompt custom fields when signing up

### Changed

- ignore boolean custom fields when checking profile completeness

## [0.3.1](https://github.com/uzh/pool/tree/0.3.1) - 2023-07-28

### Added

- link to session and experiment details in calender popup
- remove paused from partial update and create separate handlers

### Changed

- allow sending invitations to contacts without terms and conditions accepted
- many small UI/UX adjustments

## [0.3.0](https://github.com/uzh/pool/tree/0.3.0) - 2023-07-26

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

## [0.2.1](https://github.com/uzh/pool/tree/0.2.1) - 2023-06-01

### Fixed

- detailed view of tenants
- users in the root overview
- common root routes which are targeting denied/not-found via redirects
- missing delete functionality for rules

### Added

- profile pages for admins in root and tenants

## [0.2.0](https://github.com/uzh/pool/tree/0.2.0) - 2023-05-26

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

## [0.1.3](https://github.com/uzh/pool/tree/0.1.3) - 2023-03-01

### Added

- pagination, search and sort functionality for lists

### Fixed

- queue worker service context parsing

## [0.1.2](https://github.com/uzh/pool/tree/0.1.2) - 2023-02-27

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

## [0.1.1](https://github.com/uzh/pool/tree/0.1.1) - 2023-02-02

### Added

- add wysiwyg editor
- add plain text email
- hide overriden admin values from contacts
- only show user value when admin_override is disabled
- display a hint or answers of the contact when overridden
- allow non required fields to be cleared

## [0.1.0](https://github.com/uzh/pool/tree/0.1.0) - 2023-01-23

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

## [0.0.4](https://github.com/uzh/pool/tree/0.0.4) - 2023-01-13

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

## [0.0.3](https://github.com/uzh/pool/tree/0.0.3) - 2022-12-23

### Removed

- unused person table

### Fixed

- display version in footer
- verification note typo
- deployment process

## [0.0.2](https://github.com/uzh/pool/tree/0.0.2) - 2022-12-22

### Added

- option to make the matcher select the contacts in random order
- display version in footer
- CLI: create admin, grant roles and list roles

### Fixed

- create admin via tenant UI
- contact count for invitations

## [0.0.1](https://github.com/uzh/pool/tree/0.0.1) - 2022-12-16

💥 The start of using the changelog. Preparations for our pilot version 0.1.0.

### Fixed

- create operator via root UI

### Changed

- use versions without `v` prefix as we do in all our other projects
