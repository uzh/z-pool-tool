# Changelog

The format is based on [Keep a Changelog](http://keepachangelog.com/) and this project adheres to [Semantic Versioning](http://semver.org/).

## [unreleased](https://github.com/uzh/pool/tree/HEAD)

### Added

- add description to user profile completion page
- allow sessions to be closed before they end
- make pool_assignments `show_up` and `participated` nullable
- detect unsaved changes in forms
- send notification to contacts when there is an registration attempt with in-use email address

### Fixed

- removed default value from pool_sessions.start

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
