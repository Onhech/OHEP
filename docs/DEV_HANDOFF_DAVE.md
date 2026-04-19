# OHEP Dev Handoff: Prototype To Production Architecture

## Purpose
This document is the implementation handoff for Dave and the engineering team.

The current OHEP demo should be treated as:
- a visual and interaction prototype
- a page-by-page product reference
- a source of reusable HTML/CSS/UI patterns

It should **not** be treated as:
- the target application architecture
- the production data model
- the production API contract
- the final query pattern

The current demo represents the intended frontend experience and reporting behavior. The current data layer is ad hoc and precomputed for prototyping purposes and should be replaced with a structured backend architecture.

## Source Of Truth
Use these assets as the implementation reference set:

- Live prototype shell: [Demo/build/index.html](../Demo/build/index.html)
- Current generated demo data artifact: [Demo/build/demo-data.js](../Demo/build/demo-data.js)
- Visual page library: [HTML Mock-ups/INDEX.md](../HTML%20Mock-ups/INDEX.md)
- Current visual snapshots: [HTML Mock-ups/pages](../HTML%20Mock-ups/pages)

Current prototype scope:
- 27 pages
- 7 top-level sections: Getting Started, OHEP Summary, Drivers, Outcomes, Insights, Action Plan, Comments
- 7 global filter dimensions: Company, Department, Identity, Location, Employee Type, Tenure, Work Arrangement

## What Exists Today
The current system is a frontend prototype backed by a generated data bundle.

### Frontend
- Static HTML/CSS/JS shell in `Demo/app`
- Sidebar navigation, global filters, page-level controls, and page rendering
- The browser loads a large generated JS object and swaps content client-side

### Current Build Model
- `Demo/scripts/build_demo.R` reads messy source inputs and precomputes the demo
- Inputs currently include:
  - survey workbook exports
  - CSV overlays and config files
  - manually curated narrative content
  - ad hoc mappings and demo fixtures
- The build outputs a large pre-rendered artifact:
  - [Demo/build/demo-data.js](../Demo/build/demo-data.js) (`~90 MB`)

### How The Demo Works Today
- Page content is largely pre-rendered ahead of time
- Filter behavior is simulated through prebuilt outputs instead of database-backed queries
- The frontend is mostly selecting the right precomputed payload rather than requesting live data

This is acceptable for prototyping and visual review. It is not suitable as the production backend model.

## Recommended Production Architecture
Build the production version as a 5-layer system.

### A. Raw Data Layer
This is the source data coming from survey operations and curated reporting inputs:
- respondent-level survey responses
- survey metadata
- question metadata
- demographic attributes
- benchmark reference data
- narrative/theme/action-plan content
- manually curated labels and display mappings

### B. Ingestion / Transformation Layer
This layer is responsible for:
- loading raw survey exports
- validating schema and field completeness
- normalizing respondent-level data
- joining question metadata and demographic mappings
- computing derived metrics
- applying benchmark logic
- generating page-ready analytical tables
- enforcing anonymity rules and minimum-N thresholds

Implementation options:
- ETL pipeline
- scheduled jobs
- dbt / SQL transforms
- R or Python transformation jobs writing into the database

Recommendation:
- Keep heavy analytical computation here
- Do not compute everything at request time
- Publish trusted derived tables into the database

### C. Database / Analytics Store
Use a relational database. Postgres is the recommended default.

Store:
- normalized operational tables
- derived analytical tables
- filter metadata
- history and benchmark values
- curated narrative content for themes and action plans

### D. API / Backend Layer
This layer should:
- accept report id + filter context + page controls
- query the database
- enforce filter availability and anonymity thresholds
- return structured JSON payloads per page
- support pagination and search where needed

### E. Frontend Layer
The frontend should:
- preserve the current general page structures and interactions
- fetch structured page payloads from the backend
- render components from JSON
- stop depending on giant precomputed JS bundles or HTML blobs

## End-To-End Data Flow
The intended production flow is:

1. Raw survey exports and curated content are received
2. Ingestion pipeline validates and normalizes the data
3. Derived metrics and benchmark outputs are computed
4. Structured tables are stored in Postgres
5. Frontend requests page payloads from the API using active filter context
6. Backend queries the database and returns structured JSON
7. Frontend renders the charts, cards, tables, and narrative blocks

In short:
- today: frontend loads pre-rendered answers
- production: frontend queries structured page payloads

## Required Data Transformations

### 1. Survey Response Normalization
Transform raw responses into a respondent-level normalized table:
- one row per respondent per question
- respondent identifier
- survey wave / report id
- question id
- raw response value
- cleaned numeric scale value where applicable
- open-ended comment text where applicable
- demographic attributes for that respondent

Why:
- raw exports are often wide and not query-friendly
- normalized rows are the base for all downstream aggregation

### 2. Question Metadata Mapping
Create a canonical question dictionary that maps each item to:
- question id
- question text
- question type
- driver / outcome / comment classification
- parent construct
- scoring direction
- benchmark category
- display grouping used by the frontend

Why:
- the backend needs deterministic rules for how items roll up into pages and metrics

### 3. Demographic Standardization
Normalize raw respondent attributes into consistent filter dimensions:
- company
- department
- identity
- location
- employee type / manager status
- tenure
- work arrangement
- any future segment dimensions

Why:
- raw labels are often inconsistent
- filter options and availability must be stable across pages

### 4. Construct / Scale Aggregation
Calculate derived scores for:
- fundamentals / drivers
- outcomes
- item-level values
- demographic distributions
- response counts
- benchmark positions
- historical comparisons

This includes:
- scale averaging
- exclusion rules
- handling `NA` / not applicable
- agreement and disagreement percentages where needed
- metric rollups by filter slice

### 5. Benchmark Transformation
Create comparable benchmark outputs:
- percentile position
- benchmark zone classification
- benchmark-relative values
- prior-period deltas where applicable

Needed for:
- OHEP Results
- percentile displays
- zone labeling
- historical visuals

### 6. Segment Comparison Transformation
Produce comparison tables for:
- segment analysis heatmaps
- participation profile heatmaps
- demographic widget distributions
- priority matrix positions by selected segment/filter

These outputs should be queryable and filter-aware, not pre-rendered by combination.

### 7. Comment Processing Outputs
From raw comments, derive:
- topic assignment
- sentiment label
- searchable normalized text
- pagination-ready comment records
- counts by topic
- counts by sentiment
- filter-aware comment counts

If NLP is used in production, document whether sentiment/topic assignment is:
- precomputed during ingestion, or
- refreshed periodically by a downstream pipeline

### 8. Narrative Content Structuring
Store curated narrative content separately from survey calculations:
- themes
- theme summaries
- hero quotes
- verbatim quotes
- relevant metrics
- insight card copy
- action plan cards and checklist items

This content should live in content tables keyed by report id and content id, not embedded in HTML.

## Core Data Needed From Raw Responses

### Respondent-Level Inputs
For each respondent:
- respondent id or pseudonymous id
- survey wave / report id
- timestamp if relevant
- company
- department
- identity
- location
- employee type / manager-supervisor flag
- tenure
- work arrangement
- any other supported segmentation dimensions

### Question-Level Inputs
For each question:
- question id
- question text
- response type
- scale anchors if applicable
- construct mapping
- driver/outcome/comment classification
- benchmark relevance
- item grouping for display

### Response-Level Inputs
For each answer:
- respondent id
- question id
- raw response
- normalized numeric response if scale-based
- open-ended text if comment-based

### External / Curated Inputs
- benchmark reference tables
- prior-year historical values if not derived from the same response structure
- curated themes content
- curated action plan content
- display labels and ordering metadata

## Recommended Database-Oriented Entity Model
The production model should separate raw operational data, derived analytics, and curated content.

### Core Entities
- `reports`
- `survey_waves`
- `respondents`
- `questions`
- `responses`
- `filter_dimensions`
- `filter_options`

### Derived Analytical Entities
- `metric_scores`
- `metric_history`
- `benchmark_results`
- `demographic_distributions`
- `segment_comparison_values`
- `priority_matrix_points`
- `participation_profile_values`
- `comment_records`
- `comment_topic_counts`
- `comment_sentiment_counts`

### Curated Content Entities
- `themes`
- `theme_quotes`
- `theme_metrics`
- `theme_insights`
- `action_plan_focus_areas`
- `action_plan_items`

Important rule:
- raw response data should not be the only application layer
- frontend should not query raw data directly
- page payloads should be assembled from derived and curated tables

## Suggested Table Responsibilities

### Raw / Normalized Tables
Purpose:
- preserve imported source data
- support reproducible analytical transforms

Examples:
- `respondents`
- `questions`
- `responses`

### Derived Analytical Tables
Purpose:
- power page queries efficiently
- avoid repeated heavy computation at request time

Examples:
- `metric_scores`
- `metric_history`
- `benchmark_results`
- `segment_comparison_values`
- `priority_matrix_points`
- `comment_topic_counts`

### Curated Content Tables
Purpose:
- support narrative pages without embedding copy in HTML or frontend code

Examples:
- `themes`
- `theme_quotes`
- `theme_insights`
- `action_plan_focus_areas`

## Page Payloads Dev Will Need
The frontend should render page-specific JSON payloads, not HTML strings.

### OHEP Results
Payload should include:
- fundamentals grouped for display
- outcomes grouped for display
- score values
- benchmark zones
- percentile / marker values
- year-over-year delta where supported
- response count and anonymity status

Suggested shape:
- `groups`
- `metrics[]`
- `score`
- `zone`
- `percentile`
- `delta_vs_prior`
- `respondent_count`
- `is_anonymized`

### Demographics
Payload should include:
- available demographic dimensions
- selected dimensions for each widget
- counts or percentages by category
- labels and order metadata

### Priority Matrix
Payload should include:
- selected y-axis measure
- x/y coordinates per metric
- quadrant classification
- labels and metric names
- filter-aware points
- count/anonymity metadata

### Segment Analysis
Payload should include:
- compare-by dimension
- mode toggle values for drivers vs outcomes
- row definitions
- column definitions
- cell values
- color-scale bounds or backend-ready score values

### Participation Profile
Payload should include:
- row dimension
- column dimension
- participation percentages
- row/column labels
- heat-scale metadata

### Comment Explorer
Payload should include:
- topic options and counts
- sentiment summary
- filtered comments page
- pagination info
- search query support
- sentiment filter support

### Themes
Payload should include:
- title
- hero quote and subheading
- narrative paragraphs
- relevant metric pills
- insight cards
- verbatim quotes

### Action Plan
Payload should include:
- focus areas
- context paragraphs
- checklist items
- stable ordering

## Example API Endpoints
These are examples, not mandated routes. The main requirement is stable page-level contracts.

- `GET /reports/:reportId/filters`
- `GET /reports/:reportId/ohep-results`
- `GET /reports/:reportId/demographics`
- `GET /reports/:reportId/priority-matrix?y_axis=engagement`
- `GET /reports/:reportId/segment-analysis?compare_by=company&view=drivers`
- `GET /reports/:reportId/participation-profile?rows=department&cols=company`
- `GET /reports/:reportId/comments?topic=strategy&sentiment=negative&page=2&q=communication`
- `GET /reports/:reportId/themes/overview`
- `GET /reports/:reportId/themes/:themeId`
- `GET /reports/:reportId/action-plan`

All page endpoints should accept global filter context.

## Backend Responsibilities
Backend / data engineering should own:
- database schema
- ETL / ingestion pipeline
- transformation logic
- benchmark joins
- historical joins
- API layer
- anonymity enforcement
- filter validation
- page payload construction

Backend should **not**:
- generate final page HTML
- pre-render every filter combination
- embed business logic in the frontend
- use the current prototype bundle shape as the API contract

## Frontend Responsibilities
Frontend should use the prototype as the visual contract for:
- layout
- chart structure
- interaction patterns
- filter behavior
- component hierarchy
- empty and no-data states
- pagination and search UX
- page ordering and navigation

Frontend should consume:
- structured JSON payloads
- page-local controls and global filter state
- backend-provided availability and anonymity flags

## Implementation Recommendation For Dave

### Phase 1: Define Contracts
- finalize page-by-page payload contracts
- finalize normalized filter dimensions
- finalize schema for derived metrics and curated content
- decide which metrics are computed during ingestion vs query time

### Phase 2: Build Data Pipeline
- ingest raw survey exports
- normalize responses and metadata
- compute derived tables
- load Postgres
- validate counts, labels, and benchmark joins

### Phase 3: Build API
- implement page endpoints
- add filter-aware query logic
- enforce anonymity and option availability
- return structured page payloads

### Phase 4: Rebuild Frontend Data Layer
- preserve the current UX and page structures
- replace precomputed bundle lookups with API calls
- render pages from structured payloads

### Phase 5: Harden
- auth / permissions
- report versioning
- monitoring
- ingestion QA
- regression testing

## Suggested Dev Lift
Approximate effort if the current prototype remains the frontend reference.

### Light Handoff / Technical Spike
1-2 weeks
- finalize schema direction
- define page payload contracts
- stand up seed data and a few sample endpoints

### Internal MVP
3-5 weeks
- ingestion pipeline
- Postgres schema
- API for major page families
- frontend rewired to page payloads
- anonymity and filter logic

### Production-Grade Build
6-10+ weeks
- authentication and permissions
- robust ingestion and validation
- monitoring and observability
- report versioning
- fuller QA and edge-case handling

Largest lift drivers:
- cleaning and stabilizing the data model
- replacing precomputed filter combinations with query logic
- comments/themes because they combine analytical and curated data

## Acceptance Criteria For This Handoff
The handoff is complete when engineering can answer:
- what the current prototype is for
- which parts are visual reference only
- what data transformations are required
- what backend entities must exist
- what payload each page needs
- what belongs in database vs API vs frontend
- how raw survey responses become usable application data

## Assumptions
- The current prototype remains the UI/UX reference.
- The current data bundle and build scripts are not the production architecture.
- Survey calculations can be performed in an ETL layer before page requests.
- Narrative content such as themes and action plans is curated content and should be stored separately from analytical calculations.
- Postgres is the recommended database default unless engineering already has a mandated stack.

## Recommended Next Deliverables
After this document, the next concrete engineering artifacts should be:

1. Page-by-page payload contract document
2. Database schema draft
3. Seed data package aligned to that schema
4. API stub or OpenAPI spec
5. Frontend integration plan for replacing `demo-data.js`
