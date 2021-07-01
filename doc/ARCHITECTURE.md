## Z-Pool

## Context
This section describes the tool that is being built as a software product, the existing environment and the user types.

### Goal
The "Z-Pool-Tool" project aims to create an integral technical platform for all existing and future subject Pools for the DIZH universities. The "Z-Pool-Tool" should enable a technologically uniform, operationally independent recruitment of test persons as well as the administration of test person pools for all organizational units of the DIZH universities.

Users of the "Z-Pool-Tool" are therefore institutes and other DIZH organizational units that want to set up subject pools for specific purposes and maintain them according to their individual needs. The pools themselves are then available to researchers according to the specifications of the individual pools. In addition, a new DIZH volunteer pool of several 10,000 people is to be established as a strategic resource for the search for volunteers for online and offline research, independent of commercial service providers.

### User types

#### Operator
The operator's job is to run a participant pool instance. If Z-Pool is hosted on-premise, then the operator has to run the Z-Pool instance, too.

The operator is a primary user.

#### Recruiter
The recruiter creates, populates and owns a participant pool. The recruiter has almost the same capabilities as the operator, except for accessing and modifying infrastructure.

The recruiter is a primary user.

#### Experimenter
The experimenter is tasked to create and conduct experiments and consequentially owns experiments.

The experimenter is a secondary user.

#### Participant
The participant joins a participant pool and is invited to experiments by experimenters.

They are the end users of the system.

# Quality Attributes
This section summarizes the key quality attributes & the non-functional requirements.

### Performance (e.g. latency and throughput)
* When 1000 subjects sign up for an experiment at the same time after being invited, 99th percentile response time is under 500ms
* When interacting with the system as admin, the 99th percentile response time is under 1 second

### Scalability (e.g. data and traffic volumes)
* With increasing traffic, the number of queue workers and HTTP workers can be increased
* With increasing traffic, the application can be running on multiple physical hosts behind a load balancer since the processes are stateless

### Availability (e.g. uptime, downtime, scheduled maintenance, 24x7, 99.9%, etc)
- TODO What do we want to define for the system? Best-effort up-time? [andy]


### Authentication
- The default way of authentication is by providing an email address and a password
- The default password policy should be strong enough according to best practices to render brute force attacks useless

### Authorization
- Only users who where given permission to mutate some data should be able to mutate it
- Only users who where given permission to read some data should be able to read it
- Only users who where given permission to export some data should be able to export it

### Data confidentiality
- The right people have access to the right data (as defined in section "Data")

### Extensibility
- An engineers with no previous knowledge about the system but only about the technology used should be able to contribute & fix within days not weeks

### Flexibility
- The parts that face the participants is optimized as white label website which makes it easy to add custom branding
- The structure of the pools varies between primary users, which means that the data collection from participants have to be configurable (custom on-boarding process)
- The structure of the experiments varies between primary users

### Auditing
- Every activity that mutates data is logged and kept in an activity log
- Every activity that extracts a larger amount of data (CSV Export, JSON API Query) is logged and kept in an activity log

### Monitoring and management
- TODO [andy] It should be possible to monitor the health and status of the application with common monitoring tools
- The system should proactively report degraded health to operators

### Reliability
- Data integrity should be maintained by respecting defined invariants

### Failover/disaster recovery targets (e.g. manual vs automatic, how long will this take?)
- TODO [andy] How does it look right now with ZI?

### Interoperability
- It should be possible to query data using a JSON API, crawling is actively discouraged
- It should be possible to export the pool

### Legal, compliance and regulatory requirements (e.g. data protection act)
- TODO [andy] Does the system need to conform to GDPR?

### Internationalisation (i18n) and localisation (L10n)
- The participant facing part should support German and English initially
- The admin facing part should be English only
- Localisation settings should be read from the browser and it should be possible to override them in the user settings

### Accessibility
- Common best practices regarding accessibility in web development should be followed to enable everyone to participate in studies

### Usability
- Primary users should be able to create a pool with sane defaults
- Participants should be able to register initially with just an email address and a password in order to keep the friction as low as possible
- The part that is facing participants should work on mobile devices just as much as on desktop devices

### Maintanability
- An engineer who knows the technology stack but not the system should be able to contribute within days
- The system should be adaptable to changing requirements and environments

### Data consistency & integrity
- The number of duplicate participant records should be kept at a minimum with automated processes

## Constraints
This section describes the existing constraints for the project, the products and the people involved.

### Timeline
- TODO [andy] Timeline?

### Budget
- TODO [andy] Budget?

### Engineers
- TODO [andy] Engineers?

### Testing
- TODO [andy] Who is going to test and when?

### Peripheral Systems
- TODO [andy] SMS Gateway?
- TODO [andy] SMTP Server?
- TODO [andy] Webhooks?
- TODO [andy] Other constraints coming from systems we have to talk to?

## Principles
This section discusses high-level principles that guide the development of the architecture.

- The system is divided in two sub-systems; One that deals with Commands and one that deals with Queries
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

## Software Architecture
This section describes the big picture of the software containers & components and their interactions.

### Container diagram

![Overview of software containers](doc/images/container.svg "Container diagram")

#### Operator
The operators make sure that the system runs smoothly. They use the system mostly through the desktop website and they have access to the underlying infrastructure. They are the technical partners of the recruiters.

#### Recruiter
The recruiters build and own participant pools. They use the system mostly through the desktop website. They don't have access to infrastructure components.

#### Experimenter
The experimenter is assigned to experiments by recruiters. They use both the desktop and the mobile website in order to conduct experiments.

#### Participant
The participant uses the system through both the desktop and mobile website.

#### Website
The website is the entry point to the system, every user interacts with the system through it.

#### Backend
The backend serves the website and runs the business logic.

#### Infrastructure DB
This DB is used to persist data that is not related to participant data.

#### Queue Worker
The queue worker runs business logic in a separate process than the backend.

#### Cache
The backend uses the cache to persist transient data to reduce the load in the infrastructure database. The cache must not cache pool data!

#### SMS Gateway
This external service is used to send SMS.

#### E-Mail Transport
This external service is used to send emails.

#### Participant Pools
Participant pools are external systems from the point of view of the Z-Pool instance.

### Component diagram
Every component is assumed to have access to the `Main Database`, so this dependency is not explicitly listed.

![Overview of software components](doc/images/component.svg "Component diagram")

#### Command & Domain
The domain model contains all the business rules. Users interact with it by sending it commands to mutate data.

##### Settings
The settings component takes care of user settings and message data such as email templates.

##### Experiment
The experiment component takes care of managing experiments, session experiments and participations.

###### Dependencies
- `Participant Pool`
- `Messages`
- `Calendar`
- `Customization`

##### Payout
Payout lists can be downloaded or forwarded to a payout tool

###### Dependencies
- `Payout Tool`

##### Participant Pool
Participants can register and fill in their details that make them eligible for certain studies

###### Dependencies
- `Participant Pool Database`
- `Customization`

#### Query

Users send queries over HTTP to read aggregate data.

##### Dashboard
Overview of future sessions per assistant, list of assigned open experiments, list of incomplete list of sessions

##### Statistics
Calculation of response rate, number of invitations per user

#### Infrastructure

Infrastructure components don't contain any business logic and are generic enough to be re-used in other projects.

##### Authorization
The authorization component ensures that person can only do send commands and queries they are allowed to send.

###### Dependencies
- `Cache`

##### Customization
The customization component is a generic library that can be used to add custom fields to entities. The custom fields can be managed at runtime without the need to apply database schema migrations.

##### Activity Log
The activity log keeps track of all actions done by super admins, recruiters and experimenters.

##### Messaging
The messaging component implements various messaging channels such as email and SMS.

###### Dependencies
- `SMS Gateway`
- `E-Mail Transport (SMTP)`
- `Queue Worker`

##### Calendar
TODO [jerben] Check whether we have some system dependencies

The calendar component provides a calendar.

## Code
This section describes implementation details of parts of the system.

### Generating/rendering HTML
HTML is rendered on the server which is served as static HTML document to the client.

### Data binding
HTTP requests are parsed as either commands or queries. Commands and queries are then validated and authorized and passed to the service layer.
The service layer uses repositories to access the database.

### Multi-page data collection
No framework is used to collect data spanning multiple web pages. Instead, a ad-hoc case-by-case modeling of the multi-page as stateful process is required.

### Security
Use management, authentication and protection against CSRF and SQL injection is provided by [Sihl](https://github.com/oxidizing/sihl "Sihl web framework")

### Domain model
Check out [ddd.ml](ddd.ml "Domain-driven Design file")

### Customizations
Different primary users need different participant and experiement fields. Apart from a small hard-coded entity `Participant` and `Experiment`, the entities are data-driven. Because their structure is defined as persisted as data, correctness can not be checked at compile-time.

We build a customization framework that allows to attach arbitrary data to entities. It should be ergonomic to read, updte and list customized entities. Every domain component needs to use the customization framework in order to work with customized entities.

### Configuration
Z-Pool and Sihl follow the [12 Factor App](https://12factor.net/ "12 Factor App") convention. Configurations are set using environment variables or, at run-time, using Sihl's configuration service.

### Architectural layering
The architectural layering borrows concepts and from [Domain-driven design](https://martinfowler.com/tags/domain%20driven%20design.html) and [Hexagonal architecture](https://en.wikipedia.org/wiki/Hexagonal_architecture_(software)).

#### Domain
The domain is split up in entities, repositories and commands.

##### Entity
Entities are commonly referred to as "business models" or "business types". They represent things such as `Experiment`, `Invitation` or `Experimenter`. Some of the business logic lives here. They are allowed to depend on other entities only.

##### Repository
Repositories abstract away database access. They are only allowed to depend on entities of the same model. Ideally, there should be no business logic in a repository. Sometimes it makes sense to push parts of business logic into the database to improve performance.

##### Model
A model contains repositories, entities and business logic. Models are allowed to depend on other models and their entities, but never their repositories.

##### Command
A command represents a user intent to change some data. In a command handler, multiple models are called in order to run business logic.

#### Infrastructure
The infrastructure is the generic part that contains no code related to the specific problem of recruiting participents.

#### App
The app is the glue between the domain part and the infrastructure part.

### Exceptions and logging
The system logs to `stdout` and `stderr` and reports `404` and `500` via email.
Log levels of `INFO`, `DEBUG`, `WARNING` and `ERROR` can be configured.

## Data
This section discusses the data model, the persistence layer technology and data ownership.

### Data model
TODO [andy & jerben] data model

### Main Database
TODO [jerben]

### Participant Pool Database
TODO [jerben]

### Backup
Backups are not the concern of Z-Pool.

### GDPR
TODO [andy]

## Decision Log
The decision log is a list of all decisions made regarding the architecture.

### YEAR-MONTH-DAY: TITLE
#### Context
This section describes the forces at play, including technological, political, social, and project local. These forces are probably in tension, and should be called out as such. The language in this section is value-neutral. It is simply describing facts.

#### Decision
This section describes our response to these forces. It is stated in full sentences, with active voice. "We will …"

#### Status
A decision may be "proposed" if the project stakeholders haven't agreed with it yet, or "accepted" once it is agreed. If a later ADR changes or reverses a decision, it may be marked as "deprecated" or "superseded" with a reference to its replacement.

#### Consequences
This section describes the resulting context, after applying the decision. All consequences should be listed here, not just the "positive" ones. A particular decision may have positive, negative, and neutral consequences, but all of them affect the team and project in the future.
