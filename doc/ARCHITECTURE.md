# Context
This section describes the tool that is being built as a software product, the existing environment and the user types.

## Goal
The "Z-Pool-Tool" project aims to create an integral technical platform for all existing and future subject Pools for the DIZH universities. The "Z-Pool-Tool" should enable a technologically uniform, operationally independent recruitment of test persons as well as the administration of test person pools for all organizational units of the DIZH universities. Users of the "Z-Pool-Tool" are therefore institutes and other DIZH organizational units that want to set up subject pools for specific purposes and maintain them according to their individual needs. The pools themselves are then available to researchers according to the specifications of the individual pools. In addition, a new DIZH volunteer pool of several 10,000 people is to be established as a strategic resource for the search for volunteers for online and offline research, independent of commercial service providers.

## User roles
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
### GDPR
TODO describe what each right does
* The Right to Information
* The Right of Access
* The Right to Rectification
* The Right to Erasure
* The Right to Restriction of Processing
* The Right to Data Portability
* The Right to Object
* The Right to Avoid Automated Decision-Making
## Internationalisation (i18n) and localisation (L10n)
* EN/DE
## Accessibility
- TODO [andy]
## Usability
* Primary users should be able to create a pool with sane defaults
## Maintanability
- TODO small team, effectiveness long-term, mention phase planning of project

# Constraints
This section describes the existing constraints for the project, the products and the people involved.

- TODO Time, budget and resources.
- TODO Approved technology lists and technology constraints.
- TODO Existing systems and integration standards. (SMS gateway, E-Mail, Slack, ...)
- TODO Size of the software development team.
- TODO Skill profile of the software development team.

# Principles
This section describes high-level principles that guide the development of the architecture.

- TODO Architectural layering strategy. (monolith, CQRS)
- No business logic in views.
- No database access in views.
- Use of module interfaces. (information hiding)
- Never use an ORM.
- Async
- High cohesion, low coupling.
- A little copying is better than the wrong abstraction.
- Ensure all components are stateless (e.g. to ease scaling).
- Never use stored procedures.
- Common approaches for error handling, logging, etc.
- CLI first, UI second

# Software Architecture
This section describes the big picture of the software containers & components and their interactions.

- TODO What does the "big picture" look like?
- TODO Is there are clear structure?
- TODO Is it clear how the system works from the "30,000 foot view"?
- TODO Does it show the major containers and technology choices?
- TODO Does it show the major components and their interactions?
- TODO What are the key internal interfaces? (e.g. a web service between your web and business tiers)

- TODO: https://structurizr.com/help/container-diagram
- TODO: https://structurizr.com/help/component-diagram

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
