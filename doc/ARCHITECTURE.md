# Pool Tool

## Context
This section describes the tool that is being built as a software product, the existing environment and the user types.

### Goal
The "Z-Pool-Tool" project aims to create an integral technical platform for all existing and future subject Pools for the DIZH universities. The "Z-Pool-Tool" should enable a technologically uniform, operationally independent recruitment of test persons as well as the administration of test person pools for all organizational units of the DIZH universities.

Users of the "Z-Pool-Tool" are therefore institutes and other DIZH organizational units that want to set up subject pools for specific purposes and maintain them according to their individual needs. The pools themselves are then available to researchers according to the specifications of the individual pools. In addition, a new DIZH volunteer pool of several 10,000 people is to be established as a strategic resource for the search for volunteers for online and offline research, independent of commercial service providers.

### User types

#### Root
The root is responsible for running a Pool Tool instance.

#### Operator
The operators make sure that the system runs smoothly. They use the system mostly through the desktop website and they have access to the underlying infrastructure. They are the technical partners of the recruiters.

The operator is a primary user.

#### Recruiter
The recruiters build and own participant pools. They use the system mostly through the desktop website. They don't have access to infrastructure components.

The recruiter is a primary user.

#### Location manager
The location manager sees all sessions for a location and is responsible for that location. Can not update, only read.

The experimenter is a secondary user.

#### Experimenter
The experimenter is assigned to experiments by recruiters. They use both the desktop and the mobile website in order to conduct experiments.

The experimenter is a secondary user.

#### Assistant (of Experimenter)
The assistant is responsible for one session, deals with payout and checks show-up/no-show on-site.

The experimenter is a secondary user.

#### Participant
The participant joins a participant pool and is invited to experiments by experimenters. The participant uses the system through both the desktop and mobile website.

They are the end users of the system.

# Quality Attributes
This section summarizes the key quality attributes that represent the non-functional requirements.

### Performance (e.g. latency and throughput)
* When 1000 subjects sign up for an experiment at the same time after being invited, 99th percentile response time is under 500ms
* When interacting with the system as admin, the 99th percentile response time is under 1 second

### Scalability (e.g. data and traffic volumes)
* With increasing traffic, the number of queue workers and HTTP workers can be increased independently
* With increasing traffic, the application can be running on multiple physical hosts behind a load balancer since the processes are stateless

### Availability (e.g. uptime, downtime, scheduled maintenance, 24x7, 99.9%, etc)
* Scheduled maintenance should notify users
* Best-effort availability is required

### Authentication
- The default way of authentication is by providing an email address and a password
- Authentication using Edu-ID needs to be supported as well
- The default password policy should be strong enough according to best practices to render brute force attacks useless

### Authorization
- Only users who where given permission to mutate some data should be able to mutate it
- Only users who where given permission to read some data should be able to read it
- Only users who where given permission to export some data should be able to export it

### Data confidentiality
- The right people have access to the right data (as defined in section "Data")
- A tenant can not access other data of other tenants

### Extensibility
- An engineers with no previous knowledge about the system but only about the technology used should be able to contribute and fix within days not weeks

### Flexibility
- The part that face the participants is optimized as a white label website which makes it easy to add custom branding
- The structure of the pools varies between tenants, which means that the data collection from participants have to be configurable (custom on-boarding process)
- The structure of the experiments varies between tenants

### Auditing
- Every activity that mutates data is logged and kept in an activity log
- Every activity that extracts a larger amount of data (CSV Export, JSON API Query) is logged and kept in an activity log

### Monitoring and management
- The health of the system can be read using endpoints
- The system should proactively report degraded health to operators

### Reliability
- Data integrity should be maintained by respecting defined invariants

### Interoperability
- It should be possible to query data using a JSON API, crawling is actively discouraged
- It should be possible to export pools

### Legal, compliance and regulatory requirements (e.g. data protection act)
- TODO [steering committee] Does the system need to conform to GDPR?

### Internationalisation (i18n) and localisation (L10n)
- The participant facing part should support German and English
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
- The number of duplicate participant records within a pool should be kept at a minimum with automated processes

## Constraints
This section describes the existing constraints for the project, the products and the people involved.

### Peripheral Systems
- SMS Gateway (GTX Messaging), HTTP API
- TODO [ZI] UZH SMTP Server with rate limit
- MariaDB Cluster with one write instance and 3 read clones
- TODO [ZI] Edu-ID Authentication
- Gitlab API

## Principles
This section discusses high-level principles that guide the development of the architecture.

- The system is divided in two sub-systems; one that deals with Commands and one that deals with Queries
- Keep the amount of custom JavaScript at a minimum
- Separate pure code that has no side-effects from code that has side-effects
- Declarative & functional core, imperative shell
- Separate generic infrastructure components from domain components
- Use module interfaces to achieve information hiding
- Put as many business rules into pure code as possible
- End-to-end tests happy paths, unit test command handlers
- No business logic in views
- No database access in views
- Don't use an ORM but write SQL queries by hand in order to have full control over the queries
- Don't block when doing I/O
- A little copying is better than the wrong abstraction
- Ensure all components are stateless, the only stateful part it the persistence layer (and the cache)
- HTTP handlers are responsible for authorizing, parsing and calling a command handler
- Authorization is done before the command is handled, the caller of a command handler needs to call `can` as well (usually HTTP handler)
- Queries are implicit and exposed throught the model API
- A big chunk of the business logic lives pure command handler functions, test those thoroughly including edge cases
- Ideally there should be an integration test for every event
- Side effects (mutations) can only occur in event handlers
- The name of a command starts with a present verb, such as `SignUpParticipant` because is represents user intent
- The name of an event starts with the thing, such as `ParticipantSignedUp` because it represents an immutable fact
- It is not allowed to create an event in an event handler

## Software Architecture
This section describes the big picture of the software containers & components and their interactions.

### Container diagram

![Overview of software containers](doc/images/container.svg "Container diagram")

See the section "User types".

#### Website
The website is the entry point to the system, every user interacts with the system through it.

#### Load balancer
The load balancer distributes incoming traffic to multiple running web app instances.

#### Web app instance
The backend serves the website and runs the business logic.

#### Main database
This database is used to persist data that is not related to a tenant. It contains the meta data of all tenants.

#### Queue worker
The queue worker runs business logic in a separate process than the backend.

#### Cache
The backend uses the cache to persist transient data to reduce the load in the infrastructure database. The cache must not cache pool data!

#### SMS gateway
This external service is used to send SMS.

#### E-Mail transport
This external service is used to send emails.

#### Tenant database
Participant pools are external systems from the point of view of the Pool Tool instance.

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
- `Tenant Database`
- `Messaging`
- `Calendar`
- `Customization`

##### Participant
Participants can register and fill in their details that make them eligible for certain studies

###### Dependencies
- `Tenant Database`
- `Customization`

##### Tenant
Tenants are managed centrally.
- `Main Database`

#### Query
Users send queries over HTTP to read aggregate data.

##### Dashboard
Overview of future sessions per assistant, list of assigned open experiments, list of incomplete list of sessions

##### Statistics
Calculation of response rate, number of invitations per user

#### Infrastructure
Infrastructure components don't contain any business logic and are generic enough to be re-used in other projects.

##### Permission
The permission component ensures that person can only do send commands and queries they are allowed to send.

###### Dependencies
- `Cache`

##### Role
The role component provides functionality to manage permission roles.

###### Dependencies
- `Permission`

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
The calendar component provides a calendar.

## Code
This section describes implementation details of parts of the system.

### Generating/rendering HTML
HTML is rendered on the server which is served as static HTML document to the tenant.

### Data binding
HTTP requests are parsed as either commands or queries. Commands and queries are then validated and authorized and passed to the service layer. The service layer uses repositories to access the database.

### Multi-page data collection
No framework is used to collect data spanning multiple web pages. Instead, an ad-hoc case-by-case modeling of the multi-page as stateful process is required.

### Security
Use management, authentication and protection against CSRF and SQL injection is provided by [Sihl](https://github.com/oxidizing/sihl "Sihl web framework").

### Domain model
Check out [the apps](pool/app "Apps")

### Customizations
Different primary users need different participant and experiement fields. Apart from a small hard-coded entity `Participant` and `Experiment`, the entities are data-driven. Because their structure is defined as persisted as data, correctness can not be checked at compile-time.

We build a customization framework that allows to attach arbitrary data to entities. It should be ergonomic to read, updte and list customized entities. Every domain component needs to use the customization framework in order to work with customized entities.

### Configuration
Pool Tool and Sihl follow the [12 Factor App](https://12factor.net/ "12 Factor App") convention. Configurations are set using environment variables or, at run-time, using Sihl's configuration service.

### Architectural layering
The architectural layering borrows concepts from [Domain-driven design](https://martinfowler.com/tags/domain%20driven%20design.html) and [Hexagonal architecture](https://en.wikipedia.org/wiki/Hexagonal_architecture_(software)).

#### Domain
The domain is split up in entities, repositories, models and commands.

##### Entity
Entities are commonly referred to as "business models" or "business types". They represent things such as `Experiment`, `Invitation` or `Experimenter`. Some of the business logic lives here. They are allowed to depend on other entities only.

##### Repository
Repositories abstract away database access. They are only allowed to depend on entities of the same model. Ideally, there should be no business logic in a repository. Sometimes it makes sense to push parts of business logic into the database to improve performance.

##### Model
A model contains repositories, entities and business logic. Models are allowed to depend on other models and their entities, but never their repositories.

##### Command
A command represents a user intent to change some data. In a command handler, models are called in order to run business logic.

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
TODO once the domain is somewhat ready

### Database
There are two types of databases used by the Pool Tool instance.

#### Main
The main database stores meta information about the tenants and their pools. This meta information can be queried by all tenants.

#### Tenant
The tenant database stores customized participants, customized experiments, sessions, locations, permissions, calendar events and message templates.

### Backup
Any backup strategy that works with traditional RDBM can be applied.

### GDPR
TODO [steering committee]

## Decision Log
The decision log is a list of all decisions made regarding the architecture.

### OCaml vs. JS vs. Ruby vs. Python
#### Context
- A range of programming languages were evaluated considering the experience of the development team with the language and the quality attributes of Pool Tool. Among the candidates were OCaml, JavaScript, Ruby and Python.
- The order of the combined experience of the development team in descending order is: JavaScript, Ruby, OCaml, Python.
- There are projects at the Department of Economics written in each of the languages mentioned.
- It is important to fulfill the non-functional requirements regarding maintanability, extensibility, performance, reliability and data consistency & integrity.

#### Decision
OCaml was chosen because of its strict and powerful compile-time type system.

#### Consequences
- A steeper learning curve requires more effort and time spent in preparing the implementation of Pool Tool
- The initial development is slower with OCaml than with the other 3 options
- Long-term development is faster and cheaper, the longer the project exists the higher the return of invest
- Increased efficiency: the system can process a higher load using the same resources as the other 3 options

### Domain-driven development & Type-driven development vs. Top-down domain modelling
#### Context
- Given OCaml as the choice of language, type-driven development becomes possible. This approach enables developers to encode business rules into the static types in order to mathematically prove their correctness.
- The business rules of Pool Tool should be apparent, easy to understand and separated from other code.
- Manually creating costly diagrams should be kept at minimum in order to keep the iteration cycles small
- The development team has experience in Domain-driven development & Type-driven development as well as with the top-down doamin modelling approach

#### Decision
Domain model discussions are held based on code that declaratively expresses business logic instead of diagrams.

#### Consequences
- Non-technical team members need to be open to read and understand OCaml type signatures
- Business logic is clearly separated from infrastructure code (non-business logic) which increases maintanability
- The compiler mathematically proves that certain parts of the specification have been implemented correctly

### Sihl vs. JS vs. Rails vs. Django

#### Context
- All listed technologies are in use at the engineering team
- The framework that has been used most is Rails
- The legacy Pool Tool system is written in Rails
- Sihl is the only framework that uses OCaml

#### Decision
We use Sihl as the web framework.

#### Consequences
- The learning curve is steeper, since there is only a small community around Sihl
- The dependency on a web framework that is not established is risky
- Sihl allows the development to be sustainable long-term

### Monolith vs. Micro Services
#### Context
- The team has experience with both monolithic and micro service architectures
- Micro services are harder to operate, but it's easier to scale development (the number of developers)
- The team size is smaller than 5

#### Decision
A monolithic architecture is chosen.

#### Consequences
- It's easier  o deploy, operate and introspect
- Lower requirements for infrastructure
- Max team size limited

### Server-side rendering vs. Single-page application (SPA)
#### Context
- There is no dedicated UI/UX/frontend role that knows only frontend technology well
- People who develop backend are the same ones developing UI
- The ecosystem and build processes of a typical SPA add a considerable amount of overhead

#### Decision
No SPA is used, but server-side rendered pages with enhanced bits and pieces.

#### Consequences
- [HTMX](https://htmx.org/) is used where sensible to implement dynamic parts without maintaining JavaScript
- The Pool Tool can only offer basic interactive & dynamic elements
- There is no JavaScript to maintain

### SAMPLE
#### Context
This section describes the forces at play, including technological, political, social, and project local. These forces are probably in tension, and should be called out as such. The language in this section is value-neutral. It is simply describing facts.

#### Decision
This section describes our response to these forces. It is stated in full sentences, with active voice. "We will …"

#### Consequences
This section describes the resulting context, after applying the decision. All consequences should be listed here, not just the "positive" ones. A particular decision may have positive, negative, and neutral consequences, but all of them affect the team and project in the future.
