# Context
This section describes the tool that is being built as a software product, the existing environment and the user types.

## Goal
The "Z-Pool-Tool" project aims to create an integral technical platform for all existing and future subject Pools for the DIZH universities. The "Z-Pool-Tool" should enable a technologically uniform, operationally independent recruitment of test persons as well as the administration of test person pools for all organizational units of the DIZH universities.

Users of the "Z-Pool-Tool" are therefore institutes and other DIZH organizational units that want to set up subject pools for specific purposes and maintain them according to their individual needs. The pools themselves are then available to researchers according to the specifications of the individual pools.

In addition, a new DIZH volunteer pool of several 10,000 people is to be established as a strategic resource for the search for volunteers for online and offline research, independent of commercial service providers.

## User types
* Operator: super admin priviliges, primary users
* Recruiter: owns pool, almost admin except for infrastructure & system settings, primary user
* Experimenter: admin on an experiment, secondary user
* Participant: can do stuff in assigned experiements

# Quality Attributes
This section summarizes the key quality attributes & the non-functional requirements.

## Performance (e.g. latency and throughput)
* When 1000 subjects sign up for an experiment at the same time after being invited, 99th percentile response time is under 500ms
* When interacting with the system as admin, the 99th percentile response time is under 1 second

## Scalability (e.g. data and traffic volumes)
* With increasing traffic, the number of queue workers and HTTP workers can be increased
* With increasing traffic, the application can be running on multiple physical hosts behind a load balancer since the processes are stateless

## Availability (e.g. uptime, downtime, scheduled maintenance, 24x7, 99.9%, etc)
- TODO [andy]

## Security
Security is concerned with making sure that the person really is who she claims she is (authentication), making sure that a person can only do actions that she is allowed to do and that the right people can see the right data.

### Authentication
- The default way of authentication is by providing an email address and a password
- The default password policy should be strong enough according to best practices to render brute force attacks useless
### Authorization
- Only users who where given permission to mutate some data should be able to mutate it
- Only users who where given permission to read some data should be able to read it
- Only users who where given permission to export some data should be able to export it
### Data confidentiality
- The right people have access to the right data (as defined in section "Data")

## Extensibility
- An engineers with no previous knowledge about the system but only about the technology used should be able to contribute & fix within days not weeks

## Flexibility
- The parts that face the participants is optimized as white label website which makes it easy to add custom branding
- The structure of the pools varies between primary users, which means that the data collection from participants have to be configurable (custom on-boarding process)
- The structure of the experiments varies between primary users

## Auditing
- Every activity that mutates data is logged and kept in an activity log
- Every activity that extracts a larger amount of data (CSV Export, JSON API Query) is logged and kept in an activity log

## Monitoring and management
- It should be possible to monitor the health and status of the application with common monitoring tools (TODO [andy])
- The system should proactively report degraded health to operators

## Reliability
- Data integrity should be maintained by respecting defined invariants

## Failover/disaster recovery targets (e.g. manual vs automatic, how long will this take?)
- TODO [andy]

## Interoperability
- It should be possible to query data using a JSON API, crawling is actively discouraged
- It should be possible to export the pool

## Legal, compliance and regulatory requirements (e.g. data protection act)
- TODO Does the system need to conform to GDPR?

## Internationalisation (i18n) and localisation (L10n)
- The participant facing part should support German and English initially
- The admin facing part should be English only
- Localisation settings should be read from the browser and it should be possible to override them in the user settings

## Accessibility
- Common best practices regarding accessibility in web development should be followed to enable everyone to participate in studies

## Usability
- Primary users should be able to create a pool with sane defaults
- Participants should be able to register initially with just an email address and a password in order to keep the friction as low as possible
- The part that is facing participants should work on mobile devices just as much as on desktop devices

## Maintanability
- An engineer who knows the technology stack but not the system should be able to contribute within days
- The system should be adaptable to changing requirements and environments

## Data consistency & integrity
- The number of duplicate participant records should be kept at a minimum with automated processes

# Constraints
This section describes the existing constraints for the project, the products and the people involved.

## Time, budget and resources
- TODO [andy] Timeline?
- TODO [andy] Budget?
- TODO [andy] Engineers?
- TODO [andy] Testers?

## Peripheral Systems
- TODO [andy] SMS Gateway?
- TODO [andy] SMTP Server?
- TODO [andy] Webhooks?

# Principles
This section discusses high-level principles that guide the development of the architecture.

- The system is divided in two sub-systems: One that deals with Commands and one that deals with Queries
- Keep the amount of customly written JavaScript at a minimum
- Separate pure code that has no side-effects from code that has side-effects
- Declarative & functional core, imperative shell
- Separate generic infrastructure components from domain components
- Use module interfaces to achieve information hiding
- Put as many business rules into pure code as possible
- Prefer end-to-end testing to unit tests
- No business logic in views
- No database access in views
- Don't use an ORM but write SQL queries by hand in order to have full control over the queries
- Don't block when doing I/O
- High cohesion, low coupling
- A little copying is better than the wrong abstraction
- Ensure all components are stateless, the only stateful part it the persistence layer

# Software Architecture
This section describes the big picture of the software containers & components and their interactions.

## Container diagram

![Overview of software containers](doc/images/container.svg "Container diagram")

* Operator: The operators make sure that the system runs smoothly. They use the system mostly through the desktop website and they have access to the underlying infrastructure. They are the technical partners of the recruiters.
* Recruiter: The recruiters build and own participant pools. They use the system mostly through the desktop website. They don't have access to infrastructure components.
* Experimenter: The experimenter is assigned to experiments by recruiters. They use both the desktop and the mobile website in order to conduct experiments.
* Participant: The participant uses the system through both the desktop and mobile website.
* Website: The website is the entry point to the system, every user interacts with the system through it.
* Backend: The backend serves the website and runs the business logic.
* Infrastructure DB: This DB is used to persist data that is not related to participant data.
* Queue Worker: The queue worker runs business logic in a separate process than the backend.
* Cache: The backend uses the cache to persist transient data to reduce the load in the infrastructure database. The cache must not cache pool data!
* SMS Gateway: This external service is used to send SMS.
* E-Mail Transport: This external service is used to send emails.
* Participant Pools: Participant pools are external systems from the point of view of the Z-Pool instance.

## Component diagram

Every component is assumed to have access to the `Main Database`, so this dependency is not explicitly listed.

![Overview of software components](doc/images/component.svg "Component diagram")

### Command & Domain

The domain model contains all the business rules. Users interact with it by sending it commands to mutate data.

#### Settings
The settings component takes care of user settings and message data such as email templates.

#### Experiment
The experiment component takes care of managing experiments, session experiments and participations.

##### Dependencies
- `Participant Pool`
- `Messages`
- `Calendar`
- `Customization`

#### Payout
Payout lists can be downloaded or forwarded to a payout tool

##### Dependencies
- `Payout Tool`

#### Participant Pool
Participants can register and fill in their details that make them eligible for certain studies

##### Dependencies
- `Participant Pool Database`
- `Customization`

### Query

Users send queries over HTTP to read aggregate data.

#### Dashboard
Overview of future sessions per assistant, list of assigned open experiments, list of incomplete list of sessions

#### Statistics
Calculation of response rate, number of invitations per user

### Infrastructure

Infrastructure components don't contain any business logic and are generic enough to be re-used in other projects.

#### Authorization
The authorization component ensures that person can only do send commands and queries they are allowed to send.

##### Dependencies
- `Cache`

#### Customization
The customization component is a generic library that can be used to add custom fields to entities. The custom fields can be managed at runtime without the need to apply database schema migrations.

#### Activity Log
The activity log keeps track of all actions done by super admins, recruiters and experimenters.

#### Messaging
The messaging component implements various messaging channels such as email and SMS.

##### Dependencies
- SMS Gateway
- E-Mail Transport (SMTP)
- Queue Worker

#### Calendar
The calendar component provides a calendar.

# Code
This section describes implementation details of parts of the system.

## Generating/rendering HTML
TODO a short description of the framework that was created for generating HTML, including the major classes and concepts.

## Data binding
TODO the approach to updating business objects as the result of HTTP POST requests.

## Multi-page data collection
TODO a short description of the framework used for building forms that spanned multiple web pages.

## Web MVC
TODO an example usage of the web MVC framework being used.

## Security
TODO the approach to using Windows Identity Foundation (WIF) for authentication and authorisation.
Domain model: an overview of the important parts of the domain model.
Component framework: a short description of the framework that we built to allow components to be reconfigured at runtime.

## Configuration
TODO a short description of the standard component configuration mechanism in use across the codebase.

## Architectural layering
TODO an overview of the layering strategy and the patterns in use to implement it.

## Exceptions and logging
TODO a summary of the approach to exception handling and logging across the various architectural layers.

## Patterns and principles
TODO an explanation of how patterns and principles are implemented.

# Data
This section discusses the data model, the persistence layer technology and data ownership.

- TODO data model
- TODO the data is stored in MariaDB, can be owned by anyone
- TODO Backup is handled by hoster
- TODO GDPR: Long-term storage Are there any regulatory requirements for the long term archival of business data?

# Operation and Support
- TODO Is it clear how the software provides the ability for operation/support teams to monitor and manage the system?
- TODO How is this achieved across all tiers of the architecture?
- TODO How can operational staff diagnose problems?
- TODO Where are errors and information logged? (e.g. log files, Windows Event Log, SMNP, JMX, WMI, custom diagnostics, etc)
- TODO Do configuration changes require a restart?
- TODO Are there any manual housekeeping tasks that need to be performed on a regular basis?
- TODO Does old data need to be periodically archived?

# Decision Log
- TODO Why did you choose technology or framework "X" over "Y" and "Z"?
- TODO How did you do this? Product evaluation or proof of concept?
- TODO Were you forced into making a decision about "X" based upon corporate policy or enterprise architecture strategies?
- TODO Why did you choose the selected software architecture? What other options did you consider?
- TODO How do you know that the solution satisfies the major quality attributes?

TODO structure
Title These documents have names that are short noun phrases. For example, "ADR 1: Deployment on Ruby on Rails 3.0.10" or "ADR 9: LDAP for Multitenant Integration"

Context This section describes the forces at play, including technological, political, social, and project local. These forces are probably in tension, and should be called out as such. The language in this section is value-neutral. It is simply describing facts.

Decision This section describes our response to these forces. It is stated in full sentences, with active voice. "We will …"

Status A decision may be "proposed" if the project stakeholders haven't agreed with it yet, or "accepted" once it is agreed. If a later ADR changes or reverses a decision, it may be marked as "deprecated" or "superseded" with a reference to its replacement.

Consequences This section describes the resulting context, after applying the decision. All consequences should be listed here, not just the "positive" ones. A particular decision may have positive, negative, and neutral consequences, but all of them affect the team and project in the future.
