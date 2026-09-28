# Migrator

**Schema-evolution planning, validation, and migration lifecycle tooling for
PostgreSQL applications in the FUDD ecosystem.**

Migrator is a Haskell application for deriving and managing database
migrations from the evolution of SQL schema definitions.

Its central idea is different from traditional migration tools that begin
with a hand-written sequence such as:

```text
001_create_users.sql
002_add_email.sql
003_create_index.sql
```

Migrator starts from the **schema states themselves**:

```text
Git revision A
      |
      v
Schema A

Git revision B
      |
      v
Schema B

Schema A
   |
   | semantic difference
   v
Schema B
      |
      v
Migration Plan
```

Together with the companion **SqlDdl** package, the intended system can
understand what changed in a PostgreSQL schema and turn those changes into
explicit, ordered, reviewable database-evolution operations.

Migrator is responsible for questions such as:

```text
What changed?

In what order can those changes safely be applied?

Which changes are destructive?

Does a change require data transformation?

Can old and new application versions coexist during deployment?

How do we validate that the migration succeeded?

What is the rollback or remediation strategy?

What evidence should be retained after execution?
```

The current repository is an **early architectural prototype**.

At present, the implemented `delta` command parses a textual Git diff and
reports affected files and hunks. It does **not yet** perform the complete
SqlDdl-backed schema reconstruction, semantic diff, migration planning, or
database execution described in this README.

Those capabilities form the development direction of the project.

The current package version is:

```text
0.1.0.0
```

---

## Contents

- [Role in the FUDD ecosystem](#role-in-the-fudd-ecosystem)
- [Migrator versus SqlDdl](#migrator-versus-sqlddl)
- [Why Migrator exists](#why-migrator-exists)
- [Current implementation](#current-implementation)
- [Target architecture](#target-architecture)
- [Getting started](#getting-started)
- [Current CLI](#current-cli)
- [The current delta command](#the-current-delta-command)
- [Git-based schema evolution](#git-based-schema-evolution)
- [Semantic schema changes](#semantic-schema-changes)
- [Migration plans](#migration-plans)
- [Dependency ordering](#dependency-ordering)
- [Safety classification](#safety-classification)
- [Data migrations](#data-migrations)
- [Rename handling](#rename-handling)
- [Validation](#validation)
- [Rollback and remediation](#rollback-and-remediation)
- [Migration evidence](#migration-evidence)
- [Online deployment and expand/contract](#online-deployment-and-expandcontract)
- [PostgreSQL execution](#postgresql-execution)
- [Relationship with Hasql](#relationship-with-hasql)
- [Relationship with Recycler](#relationship-with-recycler)
- [Module map](#module-map)
- [Development roadmap](#development-roadmap)
- [Testing strategy](#testing-strategy)
- [Design principles](#design-principles)
- [Current limitations](#current-limitations)
- [Repository housekeeping](#repository-housekeeping)
- [License](#license)

---

# Role in the FUDD ecosystem

Database schemas are source code.

They describe persistent parts of an application's model:

```text
entities
attributes
identity
relationships
constraints
indexes
defaults
data representation
```

As application source evolves, those schemas evolve with it.

FUDD separates the problem into two major layers:

```text
                     PostgreSQL DDL
                          |
                          v
                     +---------+
                     | SqlDdl  |
                     +---------+
                     | parse   |
                     | resolve |
                     | validate|
                     | compare |
                     +----+----+
                          |
                     SchemaDiff
                          |
                          v
                    +----------+
                    | Migrator |
                    +----------+
                    | plan     |
                    | order    |
                    | assess   |
                    | validate |
                    | execute  |
                    | evidence |
                    +----+-----+
                         |
                         v
                     PostgreSQL
```

**SqlDdl understands schema structure.**

**Migrator understands schema evolution.**

This separation allows the same SqlDdl schema model to support other FUDD
systems such as:

```text
Hasql compile-time schema validation
Recycler legacy-system analysis
schema inspection
schema history
```

without requiring those consumers to depend on the migration executor.

---

# Migrator versus SqlDdl

The boundary between the two projects is fundamental.

## SqlDdl

SqlDdl should answer:

```text
What objects exist in this schema?

What are their canonical identities?

What columns and SQL types exist?

What constraints exist?

What references what?

Are the definitions internally valid?

What changed between schema A and schema B?
```

Its principal result for Migrator is a typed semantic change set:

```text
SchemaDiff
```

---

## Migrator

Migrator should answer:

```text
How should this change be performed?

Which operations depend on which others?

Can it run while the application remains online?

Could it lose data?

Does it require explicit human intent?

Does application code have to change first?

What validation proves the operation succeeded?

What happens if the migration fails?

Can it be reversed?

What execution evidence should be retained?
```

Migrator should therefore **consume** SqlDdl representations rather than
reimplement PostgreSQL parsing itself.

---

# Why Migrator exists

Traditional database migration systems commonly treat migration files as the
primary source of truth:

```text
schema state
   =
all migration scripts ever executed
```

That model is useful, but it has limitations.

Migration history can become difficult to inspect after years of changes.

Two environments can drift even if their nominal migration version is the
same.

Hand-written migrations can fail to represent the real semantic difference
between two schema definitions.

Historical systems may have incomplete migration records.

And developers frequently need to answer a simpler question:

> Given these two versions of the schema, what actually changed?

Migrator takes schema evolution itself as a first-class concept.

The desired workflow is:

```text
repository history
       |
       v
schema snapshots
       |
       v
canonical SqlDdl schemas
       |
       v
semantic differences
       |
       v
migration plans
       |
       v
review + validation
       |
       v
execution
       |
       v
evidence
```

Migration files then become **derived and reviewable execution artifacts**,
not the only representation of schema history.

---

# Current implementation

The present repository implements only the beginning of this architecture.

The current pipeline is:

```text
textual Git diff
      |
      v
diff-parse
      |
      v
[FileDelta]
      |
      v
console description
```

The application can identify whether a file in a unified diff was:

```text
created
deleted
modified
```

and report information such as:

```text
source filename
destination filename
number of hunks
binary/non-binary content
```

This is useful scaffolding for reconstructing repository history, but it is
not yet a schema migration engine.

---

## Current capability status

| Capability | Status |
| --- | --- |
| Parse unified Git diff | Implemented |
| Identify changed files | Implemented |
| Count diff hunks | Implemented |
| Recognize binary diff | Implemented |
| `help` command | Placeholder |
| `version` command | Implemented |
| Read Git commits directly | Not implemented |
| Reconstruct file at revision | Not implemented |
| Detect SQL schema files | Not implemented |
| Depend on SqlDdl | Not yet |
| Parse SQL DDL | Not implemented in Migrator |
| Reconstruct schema snapshots | Not implemented |
| Semantic schema diff | Not implemented |
| Migration operation model | Not implemented |
| Dependency graph | Not implemented |
| SQL generation | Not implemented |
| Data migration steps | Not implemented |
| Rename handling | Not implemented |
| PostgreSQL execution | Not implemented |
| Migration registry | Not implemented |
| Validation queries | Not implemented |
| Rollback/remediation | Not implemented |
| Audit/evidence | Not implemented |
| Automated test suite | Placeholder |

The repository should therefore presently be treated as an experimental
starting point.

---

# Target architecture

A useful target architecture is:

```text
                       Git repository
                            |
                            v
                   schema-file history
                            |
                            v
                     +-------------+
                     |   SqlDdl    |
                     |-------------|
                     | parse       |
                     | interpret   |
                     | resolve     |
                     | validate    |
                     +------+------+
                            |
                +-----------+-----------+
                |                       |
                v                       v
          Previous Schema          Target Schema
                |                       |
                +-----------+-----------+
                            |
                            v
                     semantic diff
                            |
                            v
                    +---------------+
                    |   Migrator    |
                    |---------------|
                    | classify      |
                    | dependencies  |
                    | plan          |
                    | validate      |
                    | evidence      |
                    +-------+-------+
                            |
                     reviewed plan
                            |
             +--------------+--------------+
             |                             |
             v                             v
       SQL operations              data transformations
             |                             |
             +--------------+--------------+
                            |
                            v
                       PostgreSQL
                            |
                            v
                     post-validation
                            |
                            v
                  migration evidence
```

The architecture deliberately contains several intermediate representations.

A migration should not jump directly from:

```text
Git diff
```

to:

```text
execute these SQL strings
```

without first understanding the schema state and the semantic meaning of the
change.

---

# Getting started

## Requirements

The current project uses:

```text
Package:        migrator-0.1.0.0
Stack snapshot: LTS 20.11
system-ghc:     true
```

The only project-specific parsing dependency currently used by Migrator is:

```text
diff-parse-0.2.1
```

The current repository does **not yet depend on SqlDdl or PostgreSQL
libraries**.

The `DB.*` modules exist but are currently empty placeholders.

---

## Clone

```bash
git clone git@github.com:whatsupfudd/migrator.git
cd migrator
```

---

## Build

```bash
stack build
```

---

## Configuration

The application currently expects a YAML configuration file.

The implementation uses the default path:

```text
~/.migrator/migrator.yaml
```

A minimal development configuration can be:

```yaml
{}
```

The configuration path can be changed using:

```text
--config
-c
```

or:

```text
migratorCONF
```

The application also reads:

```text
migratorHOME
```

although it does not currently affect substantive Migrator behaviour.

The CLI help currently describes the default configuration location as:

```text
~/.migrator/config.yaml
```

which does not match the implementation.

The implementation path should presently be treated as authoritative until
this inconsistency is corrected.

---

# Current CLI

The executable is:

```text
migrator
```

Use the standard FUDD Stack invocation form:

```bash
stack exec -- migrator <command> <arguments>
```

---

## Version

```bash
stack exec -- migrator version
```

The version command reports:

```text
package version
Git revision
Git commit date
```

---

## Help

```bash
stack exec -- migrator help
```

The command exists but the current implementation is only a placeholder.

For command-line parser help use:

```bash
stack exec -- migrator --help
```

---

## Delta

```bash
stack exec -- migrator delta <diff-file>
```

For example:

```bash
git diff HEAD~1 HEAD > /tmp/schema.diff

stack exec -- migrator delta /tmp/schema.diff
```

The current command reads and parses the diff file and describes its file
deltas.

It does not yet derive SQL migration operations.

---

# The current delta command

The current `Commands.Delta` module is the seed of Migrator's
history-understanding pipeline.

It performs:

```text
read input file
      |
      v
parseDiff
      |
      v
[FileDelta]
      |
      v
describe each delta
```

A delta currently reports:

```text
file status
source file
destination file
binary content
or number of textual hunks
```

---

## Why Git file status matters

A schema file can enter a history comparison in several ways.

### Created file

A newly created SQL file may define entirely new schema objects.

Conceptually:

```text
new schema source
      |
      v
objects added to target schema
```

### Deleted file

Objects whose definitions existed only in the deleted source may disappear
from the target schema.

That does **not** mean Migrator should blindly issue `DROP` commands.

It first needs to compare the complete before/after canonical schemas.

### Modified file

A modified schema file can contain combinations such as:

```text
new table
removed table
new column
removed column
modified constraint
modified default
type change
renaming
reformatted but semantically identical SQL
```

The textual hunk alone is insufficient to classify these safely.

This is why Git diff parsing should feed schema reconstruction rather than
direct migration generation.

---

# Git-based schema evolution

The comments in the current prototype already identify several useful Git
operations.

Conceptually, Migrator needs to perform operations equivalent to:

```bash
git diff <start-revision>..<end-revision>
```

to discover changed files,

```bash
git show --quiet <revision>^1
```

to understand ancestry,

and:

```bash
git show <revision>:<path>
```

to recover the content of a schema file at a particular revision.

The target pipeline is therefore:

```text
start revision
      |
      +----> schema source files
      |            |
      |            v
      |         SqlDdl
      |            |
      |            v
      |        Schema A
      |
end revision
      |
      +----> schema source files
                   |
                   v
                SqlDdl
                   |
                   v
               Schema B

Schema A + Schema B
        |
        v
    SchemaDiff
```

This is much more reliable than trying to interpret individual `+` and `-`
lines as standalone DDL.

---

# Schema snapshots

Migrator should treat each relevant Git state as a reproducible schema
snapshot.

A snapshot should eventually include information such as:

```text
Git commit
schema-file set
source digests
SqlDdl canonical schema
schema digest
diagnostics
```

A conceptual type might be:

```haskell
data SchemaSnapshot = SchemaSnapshot
  { revision    :: GitRevision
  , sources     :: [SchemaSource]
  , schema      :: Schema
  , digest      :: SchemaDigest
  }
```

The exact representation can evolve.

The important invariant is:

> the same source revision should reconstruct the same canonical schema.

This makes migration generation reproducible.

---

# Semantic schema changes

Migrator should operate on typed changes rather than textual SQL fragments.

A future `SchemaDiff` supplied by SqlDdl might contain concepts such as:

```text
CreateSchema
DropSchema

CreateTable
DropTable
RenameTable

AddColumn
DropColumn
RenameColumn

AlterColumnType
AlterColumnNullity
AlterColumnDefault

AddPrimaryKey
DropPrimaryKey

AddForeignKey
DropForeignKey

AddConstraint
DropConstraint

CreateIndex
DropIndex

CreateSequence
AlterSequence
DropSequence
```

Migrator should then transform those structural changes into migration
operations with lifecycle information attached.

---

## Semantic versus textual change

Consider:

```sql
CREATE TABLE account (
  uid integer
);
```

becoming:

```sql
create table account
(
    uid int4
);
```

A text diff shows changes.

A canonical schema comparison may determine that there is no relevant
database evolution.

Conversely:

```sql
amount integer
```

becoming:

```sql
amount bigint
```

is only a small text edit but may require:

```text
locking analysis
conversion analysis
dependent-object analysis
application compatibility analysis
validation
```

Migrator therefore needs the semantic schema diff from SqlDdl rather than
using Git diff as the migration model.

---

# Migration plans

The central long-term Migrator artifact should be a typed migration plan.

Conceptually:

```text
MigrationPlan
 |
 +-- identity
 +-- source schema
 +-- target schema
 +-- Git provenance
 +-- operations
 +-- dependency graph
 +-- compatibility information
 +-- validation
 +-- rollback/remediation
 `-- evidence requirements
```

A plan is more than a list of SQL strings.

---

## Migration operations

A migration operation should carry information such as:

```text
operation identity
schema change that caused it
SQL or executable action
dependencies
transaction policy
risk/safety classification
compatibility implications
validation checks
rollback/remediation
source provenance
```

Conceptually:

```haskell
data MigrationStep = MigrationStep
  { stepId       :: StepId
  , change       :: SchemaChange
  , action       :: MigrationAction
  , dependsOn    :: Set StepId
  , safety       :: SafetyClass
  , validation   :: [Validation]
  , remediation  :: RemediationPlan
  }
```

The specific types should evolve with the implementation.

---

# Dependency ordering

Migration operations cannot always execute in arbitrary order.

For example:

```text
create parent table
       |
       v
create child table
       |
       v
create foreign key
```

Likewise, removal may require the reverse:

```text
drop foreign key
       |
       v
drop dependent object
       |
       v
drop referenced column
```

Migration planning should therefore produce a **dependency graph**, not just
rely on the order in which textual differences were encountered.

---

## Topological execution

For independent steps:

```text
        +--> step B --+
step A -+             +--> step D
        +--> step C --+
```

Migrator can derive a topological execution order.

This creates a future opportunity for parallel execution where PostgreSQL
semantics and lock behaviour permit it, while preserving explicit
dependencies.

Correctness should come before parallelism.

---

# Safety classification

Schema changes have very different operational properties.

Migrator should classify them explicitly.

A useful model can distinguish concepts such as:

```text
non-destructive structural change
compatibility-sensitive change
locking/performance-sensitive change
potentially destructive change
requires data transformation
requires explicit intent
unsupported / ambiguous
```

This classification is not intended to replace human review.

It makes the reasons for review machine-visible.

---

## Typical lower-risk examples

Depending on PostgreSQL version and application behaviour:

```text
adding a nullable column
adding some indexes
creating an independent table
creating a new sequence
```

may be straightforward.

Migrator should still derive validation and dependency requirements.

---

## Potentially destructive operations

Examples include:

```text
DROP TABLE
DROP COLUMN
type narrowing
constraint strengthening
removing enum/domain values
removing objects still used by deployed application versions
```

These operations must remain explicit in the plan.

They should never be hidden inside generic generated SQL.

---

# Data migrations

Some schema evolution cannot be represented by DDL alone.

Consider:

```text
before:

customer
  full_name text

after:

customer
  first_name text
  last_name text
```

The structural difference is easy to identify:

```text
drop full_name
add first_name
add last_name
```

but executing that literal transformation would destroy information.

The required migration is closer to:

```text
1. add first_name
2. add last_name
3. transform existing data
4. verify transformed rows
5. update application readers/writers
6. remove full_name only when safe
```

The data transformation cannot be inferred reliably from the DDL.

Migrator must therefore support **explicit data-migration steps**.

---

## Migration action types

A mature plan may need operations such as:

```text
DDL action
SQL data action
Haskell migration action
validation query
manual checkpoint
deployment checkpoint
external action
```

Not every migration should be forced into one SQL string.

---

## Explicit transformation code

For complex transformations, Haskell is a natural implementation option
inside the FUDD ecosystem.

A migration step can use:

```text
Hasql
transaction handling
streaming/batched processing
typed domain logic
```

while still participating in the same migration plan and evidence model.

---

# Rename handling

Renames are one of the most dangerous cases for automatically generated
migrations.

Given:

```text
old schema:

customer.full_name
```

and:

```text
new schema:

customer.display_name
```

the structural diff could mean either:

```text
DROP full_name
ADD display_name
```

or:

```text
RENAME full_name TO display_name
```

Those operations have very different data consequences.

---

## Do not silently guess

Heuristics can identify rename candidates using evidence such as:

```text
similar names
same SQL type
same constraints
same position
same dependent objects
same Git commit
```

but a candidate is not proof.

Migrator should represent uncertain cases explicitly:

```text
PossibleRename
  old = ...
  new = ...
  evidence = ...
```

and require explicit confirmation or migration metadata where ambiguity
would otherwise risk data loss.

---

# Table splits and merges

More complex transformations include:

```text
one table -> several tables

several tables -> one table
```

or changing relationship normalization.

These cannot safely be modeled as independent create/drop operations.

The migration plan needs to represent:

```text
new structures
data-copy/transformation steps
temporary compatibility state
validation
old-structure retirement
```

This is one of the reasons Migrator's target scope is broader than simply
rendering `SchemaDiff` to SQL.

---

# Validation

Every significant migration should answer:

> How do we know that it worked?

Validation should be part of the migration definition rather than an
afterthought.

---

## Pre-migration validation

Examples:

```text
expected source schema digest matches
target object does not unexpectedly already exist
data satisfies prerequisites
no invalid/null values block the transformation
application compatibility conditions hold
```

---

## Step validation

A step can contain assertions such as:

```sql
SELECT count(*)
FROM customer
WHERE first_name IS NULL;
```

with an expected result.

---

## Post-migration validation

Examples include:

```text
actual schema matches expected target schema
all expected constraints exist
foreign keys validate
indexes exist
row counts satisfy invariants
transformed values satisfy business rules
old structures are retired only when intended
```

The strongest generic validation is:

```text
introspected live schema
        |
        v
canonical SqlDdl model
        |
        v
compare
        |
        v
expected target schema
```

---

# Rollback and remediation

"Rollback" is not synonymous with:

```text
run the generated SQL backwards
```

Some operations are inherently irreversible once data has been discarded.

Migrator should therefore distinguish several cases.

---

## Reversible migration

A reliable reverse operation exists.

For example, some additive structural changes can be removed if no dependent
state has been created.

---

## Recoverable migration

Direct reversal is not safe, but recovery is possible through:

```text
backup
shadow copy
preserved old column
retained old table
captured mapping data
```

---

## Remediation-only migration

The operation cannot realistically be reversed automatically.

The migration definition should instead document what to do after failure.

---

## Irreversible migration

Some destructive operations should be explicitly labeled irreversible and
require deliberate approval before execution.

Migrator must never manufacture a false rollback guarantee.

---

# Migration evidence

For production use, a migration should produce durable evidence.

Useful information includes:

```text
migration identity
source Git revision
target Git revision
source schema digest
target schema digest
planned operations
actual operations
start/end timestamps
application/build identity
database identity
validation results
warnings
operator/automation identity
failure information
rollback/remediation information
```

This turns migration execution into an auditable engineering event.

---

## Desired migration package

A generated migration artifact should eventually contain enough information
to review the migration without reverse-engineering it from SQL.

Conceptually:

```text
migration/
 |
 +-- plan
 +-- generated SQL
 +-- explicit data transformations
 +-- validation queries
 +-- compatibility notes
 +-- rollback/remediation
 `-- provenance
```

The exact physical representation can evolve.

---

# Online deployment and expand/contract

Database migrations often occur while application processes are still
running.

That creates a compatibility problem:

```text
old application version
        +
new database schema
        +
new application version
```

may coexist during deployment.

Migrator therefore needs to understand migration **phases**, not merely
before and after.

---

## Expand

First introduce backward-compatible structures.

Examples:

```text
add a nullable column
add a new table
add an index
add a new representation alongside the old one
```

Old application versions should continue to function.

---

## Migrate / activate

Deploy code that understands the new structures and, where necessary:

```text
backfill data
dual-write
switch reads
validate
```

---

## Drain

Allow old application generations and outstanding work to finish.

---

## Contract

Only then remove obsolete structures:

```text
old columns
old tables
temporary compatibility triggers
obsolete constraints
```

This can be represented as:

```text
EXPAND
   |
   v
DEPLOY / MIGRATE
   |
   v
DRAIN
   |
   v
CONTRACT
```

A production deployment system should not blindly execute destructive
contract operations during a hot application reload.

---

# Migration compatibility

A mature migration step should be able to state which application
generations it is compatible with.

For example:

```text
step: add customer.display_name

compatible with:
  old readers       yes
  old writers       yes
  new readers       yes
  new writers       yes
```

versus:

```text
step: drop customer.full_name

compatible with:
  old readers       no
  old writers       no
  new readers       yes
  new writers       yes
```

This information can become important to FUDD's application deployment and
Wapp lifecycle systems.

---

# PostgreSQL execution

The current repository contains no PostgreSQL execution implementation.

`DB.Connect` and `DB.Opers` are empty modules.

The target execution layer should be PostgreSQL-first and use FUDD's normal
database stack:

```text
Hasql
Hasql.Transaction
Hasql.Pool
```

where appropriate.

---

## Transaction boundaries

Not every migration should automatically be wrapped in one enormous
transaction.

Some PostgreSQL operations have special transactional or locking behaviour.

The plan should therefore make transaction policy explicit.

Possible concepts include:

```text
same transaction as previous step
own transaction
must run outside transaction
batched transactional data migration
manual checkpoint
```

The planner must understand the semantics of the concrete PostgreSQL
operation rather than applying one universal policy.

---

# Relationship with Hasql

Migrator and Hasql solve different layers of the database problem.

```text
SqlDdl
    |
    v
schema understanding

Migrator
    |
    v
schema evolution

Hasql
    |
    v
typed PostgreSQL execution
```

Migrator can use Hasql for:

```text
running validation statements
executing generated/static SQL
running data transformations
reading migration state
writing evidence
```

but Hasql should not become Migrator's schema-diff engine.

---

## Relationship with schema-aware Hasql-TH

A shared SqlDdl schema representation also creates an important consistency
property.

The schema that Migrator expects to deploy can be the same schema used by
schema-aware Hasql-TH to validate application SQL at compile time:

```text
                   SqlDdl Schema
                    /          \
                   /            \
                  v              v
             Migrator       Hasql-TH
                |           validation
                v              |
            database           v
                         application SQL
```

That reduces the risk that compile-time SQL assumptions and migration
planning evolve independently.

---

# Relationship with Recycler

Recycler deals with understanding and modernising legacy applications.

Legacy databases may provide:

```text
current SQL dump
partial migration history
Git history
application queries
ORM definitions
```

SqlDdl can reconstruct canonical schema states.

Migrator can then reason about how to move from:

```text
legacy schema
```

to:

```text
target FUDD schema
```

while preserving data.

A Recycler modernization flow can therefore become:

```text
legacy source + database
          |
          v
       Recycler
          |
          +------> SqlDdl schema understanding
          |
          v
     target schema
          |
          v
       Migrator
          |
          v
reviewed transformation plan
```

Complex legacy transformations will typically require explicit data
migration logic rather than automatic DDL generation alone.

---

# Migration identity and checksums

Once Migrator executes migrations, it should assign a stable identity to
each migration artifact.

Useful fields include:

```text
migration ID
source revision
target revision
plan digest
source schema digest
target schema digest
content checksum
```

Applied migrations should not be silently mutated.

If an already-applied migration artifact changes, the runtime should detect
the checksum mismatch and refuse to pretend that the historical execution
matches the changed source.

---

# Live-schema verification

Before applying a generated plan, Migrator should verify that the database
is actually in the expected source state.

Conceptually:

```text
expected Schema A
       |
       | compare
       v
live PostgreSQL schema
       |
       +---- equal ---> continue
       |
       `---- differs -> stop / investigate
```

Without this check, Migrator may generate a correct:

```text
A -> B
```

plan but accidentally execute it against:

```text
A'
```

because of manual changes or incomplete earlier migrations.

Failing closed is preferable.

---

# Plan versus execution

Migrator should separate planning from application.

A typical future CLI could expose workflows conceptually similar to:

```text
migrator plan
migrator show
migrator validate
migrator apply
migrator verify
```

These commands are architectural direction only; they are not present in the
current repository.

The separation is important.

CI may be allowed to:

```text
generate
inspect
validate
```

a migration without having credentials capable of changing a production
database.

---

# Dry-run and review

Migration plans should be reviewable before execution.

A useful plan presentation can include:

```text
source schema
target schema

schema changes

ordered execution steps

destructive operations

explicit data transformations

expected locks / special execution concerns

compatibility phases

validation

rollback/remediation
```

A dry-run should generate and validate the plan without mutating the target
database.

---

# Idempotency

Migration execution should be designed so that an interrupted operation has
a well-defined recovery path.

This does not necessarily mean every SQL statement itself must be
idempotent.

It means Migrator should know:

```text
which step started
which step completed
which validation passed
what state remains after failure
```

and be able to refuse unsafe blind re-execution.

---

# Module map

The current codebase is intentionally small.

## Commands

| Module | Current responsibility |
| --- | --- |
| `Commands` | Re-exports command implementations |
| `Commands.Delta` | Parse and describe unified Git diff files |
| `Commands.Help` | Placeholder help implementation |
| `Commands.Version` | Package/Git version reporting |

## Application control

| Module | Current responsibility |
| --- | --- |
| `MainLogic` | Runtime option creation and command dispatch |
| `Options` | Configuration merge |
| `Options.Cli` | CLI definitions |
| `Options.ConfFile` | YAML configuration |
| `Options.Runtime` | Effective runtime configuration |

## Database

| Module | Current responsibility |
| --- | --- |
| `DB.Connect` | Empty placeholder |
| `DB.Opers` | Empty placeholder |

The current module map reflects the project's prototype stage.

A mature architecture will likely require explicit modules for:

```text
Git history
schema snapshots
SqlDdl integration
schema differences
migration plans
dependency graphs
safety analysis
data migrations
validation
execution
evidence
registry/history
rendering
```

---

# Proposed module architecture

One possible future organization is:

```text
Migrator.Git.*
    repository/history access
    schema-source reconstruction

Migrator.Schema.*
    SqlDdl adapters
    snapshots
    digests

Migrator.Diff.*
    migration-oriented interpretation of SchemaDiff

Migrator.Plan.*
    migration operations
    dependency graph
    phase planning
    safety classification

Migrator.Data.*
    explicit data transformations

Migrator.Validate.*
    preconditions
    step validation
    postconditions

Migrator.Render.*
    human-readable plan
    generated SQL
    machine-readable plan

Migrator.Execute.*
    Hasql/PostgreSQL execution
    transaction policy

Migrator.Store.*
    applied migration registry
    evidence

Migrator.Rollback.*
    reverse/remediation model
```

The exact namespace is not important yet.

The separation of responsibilities is.

---

# Development roadmap

## Phase 0 — Stabilize the existing prototype

Before adding migration execution:

```text
real automated tests
CI build
clean configuration behaviour
remove generic-template leftovers
document Git diff input
structured delta output
```

The existing Git-diff functionality should become deterministic and tested.

---

## Phase 1 — Git schema reconstruction

Implement:

```text
repository discovery
revision selection
parent/revision traversal
schema-file identification
file-at-revision retrieval
source digests
```

Produce reproducible schema-source snapshots.

The current `Commands.Delta` notes already contain useful Git operations
for this work.

---

## Phase 2 — Integrate SqlDdl

Replace SQL-specific inference from textual diff lines with:

```text
schema source
     |
     v
SqlDdl
     |
     v
canonical Schema
```

Generate `Schema A` and `Schema B` from two revisions.

At this stage Migrator should no longer need to understand the internals of
PostgreSQL DDL syntax.

---

## Phase 3 — Consume semantic SchemaDiff

Use SqlDdl to produce typed structural differences.

Build Migrator's internal:

```text
MigrationIntent
MigrationStep
MigrationPlan
```

types.

Keep these independent from rendered SQL.

---

## Phase 4 — Dependency planning

Build a dependency graph for changes such as:

```text
tables
columns
foreign keys
indexes
sequences
constraints
```

Detect cycles and unresolved dependencies explicitly.

Produce a deterministic topological plan where possible.

---

## Phase 5 — Safety analysis

Add explicit classification for:

```text
destructive operations
lock-sensitive operations
compatibility-breaking changes
ambiguous renames
data-transform requirements
unsupported operations
```

Require explicit intent where automation cannot establish safety.

---

## Phase 6 — Generated SQL and plan rendering

Produce:

```text
human-readable migration report
machine-readable migration plan
PostgreSQL execution SQL
validation SQL
```

Keep generated SQL traceable to the originating schema changes.

---

## Phase 7 — Explicit data migrations

Allow application-defined transformation steps.

Support at least:

```text
SQL transformations
Hasql/Haskell transformations
validation checkpoints
manual checkpoints
```

A schema diff must be able to declare:

```text
cannot complete safely without explicit data step
```

rather than guessing one.

---

## Phase 8 — PostgreSQL execution

Add:

```text
Hasql connection/pool
migration registry
transaction policy
step execution
failure handling
concurrency protection
```

Execution should refuse to start unless the live database matches the plan's
expected source schema.

---

## Phase 9 — Validation and evidence

Require migrations to include:

```text
preconditions
postconditions
affected application statements where available
compatibility implications
rollback or remediation
execution evidence
```

Persist sufficient evidence to reconstruct what happened.

---

## Phase 10 — Deployment lifecycle

Add explicit support for:

```text
expand
activate/migrate
drain
contract
```

so schema evolution can coordinate with live FUDD application generations.

---

## Phase 11 — Historical analysis

Once schema snapshots and diffs are reliable, derive:

```text
schema lineage
table lineage
column lineage
historical migration plans
change frequency
legacy migration reconstruction
```

from Git history.

This becomes particularly useful to Recycler.

---

# Testing strategy

The current test suite is only a placeholder.

A migration system requires unusually strong testing because errors can
cause persistent data loss.

---

## Git-diff tests

Test the current implementation with:

```text
created file
deleted file
modified file
rename
multiple hunks
binary file
multiple files
malformed diff
```

---

## Schema-snapshot tests

Once SqlDdl integration exists:

```text
revision -> source set
source set -> canonical schema
same revision -> same digest
```

should be deterministic.

---

## Schema-diff tests

Cover:

```text
create/drop table
add/drop column
type changes
nullability changes
default changes
keys
foreign keys
indexes
constraints
sequences
renames
```

---

## Migration-plan golden tests

For a known:

```text
Schema A -> Schema B
```

store the approved:

```text
changes
dependency graph
ordered steps
safety classifications
generated SQL
validation
```

as golden fixtures.

---

## Property tests

Useful invariants include:

```text
diff S S = no changes

empty diff -> empty migration plan

all dependency edges precede dependents in execution order

every destructive schema change remains visible in the plan

every migration step is traceable to an originating change
```

---

## PostgreSQL integration tests

Run plans against disposable PostgreSQL instances.

Validate:

```text
schema before
apply
schema after
```

against the expected canonical SqlDdl schemas.

---

## Failure-injection tests

Intentionally fail:

```text
before a step
during a data transformation
after SQL but before registry write
during validation
during rollback/remediation
```

and verify that Migrator reports a deterministic recoverable state.

---

## Compatibility tests

For expand/contract migrations, run both old and new application query sets
against intermediate schema states.

This is much stronger than merely checking whether the DDL executed.

---

# Design principles

## Schema states are the foundation

Migration files are useful artifacts, but canonical before/after schema
states provide the strongest basis for deciding what structurally changed.

---

## Git diff is evidence, not semantics

A Git diff tells Migrator where source changed.

SqlDdl tells Migrator what the schema change means.

Do not conflate the two.

---

## PostgreSQL first

FUDD's database stack is PostgreSQL/Hasql.

Migrator should prefer deep understanding of PostgreSQL migration semantics
over superficial multi-database support.

---

## Plans are typed data

Do not reduce the migration plan to:

```haskell
[Text]
```

containing SQL strings.

Dependencies, destructive changes, validation, provenance, and lifecycle
phases should remain structured information.

---

## Fail closed

If Migrator cannot determine that a migration is understood sufficiently to
execute safely:

```text
stop
```

and require explicit input.

---

## Never infer destructive intent silently

Particularly for:

```text
renames
drops
type narrowing
table splits
table merges
```

a plausible heuristic is not sufficient authorization to destroy data.

---

## Preserve provenance

Every generated step should be traceable to:

```text
source revision
target revision
schema objects
semantic change
source definitions
```

where possible.

---

## Separate planning from execution

Planning should be pure or close to pure.

Database access belongs at the execution/verification boundary.

This makes migration logic easier to test and permits CI review without
production credentials.

---

## Separate structural and data migration

SqlDdl can establish structural differences.

Migrator can determine that a data transformation is required.

Application/domain knowledge must provide transformations that cannot be
derived safely from schema structure.

---

## Rollback claims must be truthful

If data has been destroyed, generating the syntactic opposite DDL does not
restore it.

Prefer an explicit remediation plan over a fictional reversible migration.

---

## Validation belongs in the migration

A migration is not finished because PostgreSQL returned success.

It is finished when required postconditions have been demonstrated.

---

## Do not couple hot reload to destructive migration

Application deployment may be frequent.

Destructive database changes should move through explicit lifecycle phases
rather than being run automatically simply because a new binary or Wapp
version appeared.

---

# Current limitations

The current repository has several important limitations.

## It does not yet depend on SqlDdl

Despite SqlDdl being the intended downstream schema source, today's package
depends on:

```text
diff-parse
```

only.

The SqlDdl integration described in this README is future work.

---

## `delta` only understands textual Git diff structure

It does not interpret SQL statements or derive schema changes.

---

## No Git repository operations are implemented

The code comments describe useful `git diff` and `git show` operations, but
the application currently expects the caller to provide a diff file.

---

## No migration model exists

There are no current types representing:

```text
schema change
migration step
migration plan
dependency
validation
rollback
```

---

## No PostgreSQL execution exists

`DB.Connect` and `DB.Opers` are empty.

---

## No migration registry exists

Migrator currently records no applied migration state or checksums.

---

## No schema verification exists

The live database is not compared with an expected source or target schema.

---

## No data migration mechanism exists

There is currently no mechanism for explicit transformations.

---

## No rollback mechanism exists

Nor is there currently a remediation model.

---

## No tests exist

The test executable currently prints:

```text
Test suite not yet implemented
```

---

## Configuration is mostly template infrastructure

The only effective runtime option is currently:

```text
debug
```

and even that is not used by the domain logic.

---

## Configuration-path documentation is inconsistent

The CLI help says:

```text
~/.migrator/config.yaml
```

while the implementation uses:

```text
~/.migrator/migrator.yaml
```

---

# Repository housekeeping

The package metadata still references the older:

```text
hugodro/migrator
```

repository.

These values should be updated to:

```text
whatsupfudd/migrator
```

for:

```text
GitHub repository
homepage
bug reports
source repository
README URL
```

The current copyright metadata also predates the
current FUDD project organization and should be reviewed.

---

## README

The current README contains only:

```markdown
# migrator
```

and therefore does not explain either the prototype or the intended
architecture.

This document is intended to replace it.

---

## Changelog

The current changelog contains only the generated initial release skeleton.

Development milestones should begin recording transitions such as:

```text
Git reconstruction
SqlDdl integration
SchemaDiff support
migration planning
dependency graph
data migrations
PostgreSQL execution
validation/evidence
deployment lifecycle
```

---

## Package dependencies

As SqlDdl integration begins, the package should depend on the actual SqlDdl
library rather than duplicating DDL/parser types.

PostgreSQL/Hasql dependencies should be introduced when the execution
boundary is implemented, rather than adding database coupling to otherwise
pure planning modules.

---

# Long-term position

Migrator should become FUDD's **database-evolution compiler and lifecycle
engine**.

A useful analogy is:

```text
source code compiler

source
  ->
AST
  ->
semantic analysis
  ->
execution plan
  ->
machine code
```

versus:

```text
schema evolution

DDL revisions
  ->
SqlDdl schemas
  ->
semantic schema diff
  ->
migration plan
  ->
PostgreSQL operations
```

But database evolution adds one critical constraint:

```text
the previous state contains valuable persistent data
```

so the transformation cannot be treated as ordinary code generation.

The durable architecture should therefore be:

```text
                       Git
                        |
                        v
                schema source states
                        |
                        v
                    SqlDdl
                        |
             +----------+----------+
             |                     |
             v                     v
        Schema A                Schema B
             |                     |
             +----------+----------+
                        |
                        v
                   SchemaDiff
                        |
                        v
                  +-----------+
                  | Migrator  |
                  |-----------|
                  | classify  |
                  | plan      |
                  | order     |
                  | validate  |
                  | protect   |
                  +-----+-----+
                        |
              reviewed migration
                        |
          +-------------+-------------+
          |                           |
          v                           v
     structural SQL             data migration
          |                           |
          +-------------+-------------+
                        |
                        v
                    PostgreSQL
                        |
                        v
                   verification
                        |
                        v
                     evidence
```

That division gives FUDD one coherent answer to three related questions:

```text
SqlDdl:
    What does the schema mean?

Migrator:
    How does one schema safely become another?

Hasql:
    How does application code interact with the resulting database?
```

The immediate next milestone is therefore not to build a generic SQL script
runner.

It is to complete the chain:

```text
Git revisions
     ->
schema source snapshots
     ->
SqlDdl canonical schemas
     ->
semantic SchemaDiff
     ->
typed MigrationPlan
```

Once that planning layer is trustworthy, PostgreSQL execution, data
migration, expand/contract deployment, rollback/remediation, and audit
evidence can be added without collapsing all responsibilities into one
opaque command.

---

# License

The package declares the **BSD-3-Clause** license.

See the repository's `LICENSE` file for the authoritative licensing terms.