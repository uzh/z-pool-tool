# Changelog

The format is based on [Keep a Changelog](http://keepachangelog.com/) and this project adheres to [Semantic Versioning](http://semver.org/).

## [unreleased](https://github.com/uzh/pool/tree/HEAD)

### Added

- reply-to address to emails, using tenant contact email address (system settings)
- buttons to reset forms
- validation for tenant database urls, when creating or updating a tenant

### Changed

- when signing up for a session with follow-ups, the contact receives one confirmation message containing all sessions
- using uuids instead of ids as foreign keys in assignment table
- allow contacts to resignup after session cancellation
- tenant description, icon and styles are optional
- refactor and update guardian integration
- using uuids instead of ids as foreign keys in invitations and mailings tables

### Fixed

- filter by boolean fields
- custom fields with the option 'admin_view_only' will automatically have 'admin_input_only' set to true
- partial update of 'admin_view_only' fields
- when cancelling an assignment, assignments of the same contact to follow-up sessions will be canceled as well
- canceled assignments are not included in assignment count anymore
- canceled assignments are no longer shown on the contact dashboard
- when cancelling a session, follow-up sessions to the session are canceled as well
- do not include canceled assignments in session assignment_count

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
