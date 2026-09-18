-- Resource v1: standalone interval blocks. Every fact is [start, end),
-- start inclusive, end exclusive. Two records for the same subject never
-- overlap, enforced by GiST exclusion constraints over a generated tsrange.

create extension if not exists btree_gist;

-- ENUM TYPES (members spell the Scala Enumerated tags)

create type e_site as enum('gn', 'gs');
comment on type e_site is 'Observatory site.';

create type e_telescope_availability as enum('Open', 'Closed');
comment on type e_telescope_availability is 'Whether the telescope is open or closed.';

create type e_too_support as enum('Standard', 'Interrupt', 'Rapid', 'None');
comment on type e_too_support is 'Level of Target of Opportunity support.';

create type e_telescope_mode_type as enum('Queue', 'Classical', 'PriorityVisitor', 'Engineering', 'Commissioning', 'Shutdown', 'BlockScheduling');
comment on type e_telescope_mode_type is 'Telescope observing mode type.';

-- Members are the lucuma-core Partner tags, which the ODB also stores.
create type e_partner as enum('ar', 'br', 'ca', 'cfh', 'cl', 'gt', 'keck', 'kr', 'lp', 'subaru', 'uh', 'us');
comment on type e_partner is 'A Gemini partner, by lucuma-core tag.';

-- DOMAINS
--
-- Every text column below is read back through a codec that rejects an
-- unparseable value. A bad row therefore fails each read of its table, not
-- only the read of that row. These domains reject the bad value at write
-- time instead.

create domain d_semester as varchar
  check (value ~ '^\d{4}[AB]$');
comment on domain d_semester is 'A formatted Semester, for example 2026A.';

-- A text[] column cannot use a domain for its elements, because Postgres reports
-- an array of a domain under its own type name and the skunk text[] codec then
-- rejects the column. This function gives the array columns the same protection.
create function f_array_matches(arr text[], pattern text) returns boolean
  language sql immutable strict as $$
    select coalesce(bool_and(x is not null and x ~ pattern), true) from unnest(arr) as x
  $$;
comment on function f_array_matches(text[], text) is 'True when every element of arr matches pattern. Empty array is true.';

-- TOO SUPPORT BLOCKS

create table t_too_support_block (
  c_id          bigint generated always as identity primary key,
  c_site        e_site        not null,
  c_start       timestamp     not null,
  c_end         timestamp     not null,
  c_interval    tsrange       not null generated always as (tsrange(c_start, c_end, '[)')) stored,
  c_too_support e_too_support not null,
  c_note        text          null check (c_note is null or char_length(c_note) > 0),

  constraint too_interval_order check (c_start < c_end),
  constraint too_no_overlap exclude using gist (c_site with =, c_interval with &&)
);
comment on table t_too_support_block is 'ToO support level over an interval, per site.';

-- The night projection asks for the overlap as `c_interval && tsrange(...)`, which the
-- GiST index behind the exclusion constraint above serves. The flat block queries go
-- through Grackle, whose predicates cannot express `&&`, so they ask for the same overlap
-- as `c_end > $start and c_start < $end`. This btree index serves that form.
create index i_t_too_support_block_site_start on t_too_support_block (c_site, c_start);

-- TELESCOPE AVAILABILITY BLOCKS

create table t_telescope_availability_block (
  c_id           bigint generated always as identity primary key,
  c_site         e_site                   not null,
  c_start        timestamp                not null,
  c_end          timestamp                not null,
  c_interval     tsrange                  not null generated always as (tsrange(c_start, c_end, '[)')) stored,
  c_availability e_telescope_availability not null,
  c_port         integer                  null check (c_port > 0),
  c_reason       text                     null check (c_reason is null or char_length(c_reason) > 0),
  c_note         text                     null check (c_note is null or char_length(c_note) > 0),

  constraint availability_interval_order check (c_start < c_end),
  -- Subject is the port; 0 stands in for "whole telescope" (c_port null),
  -- because NULLs never collide in an exclusion constraint.
  constraint availability_no_overlap exclude using gist (c_site with =, (coalesce(c_port, 0)) with =, c_interval with &&)
);
comment on table t_telescope_availability_block is 'Telescope or port availability over an interval, per site.';

-- The night projection asks for the overlap as `c_interval && tsrange(...)`, which the
-- GiST index behind the exclusion constraint above serves. The flat block queries go
-- through Grackle, whose predicates cannot express `&&`, so they ask for the same overlap
-- as `c_end > $start and c_start < $end`. This btree index serves that form.
create index i_t_telescope_availability_block_site_start on t_telescope_availability_block (c_site, c_start);

-- TELESCOPE MODE BLOCKS

create table t_telescope_mode_block (
  c_id                 bigint generated always as identity primary key,
  c_site               e_site                not null,
  c_start              timestamp             not null,
  c_end                timestamp             not null,
  c_interval           tsrange               not null generated always as (tsrange(c_start, c_end, '[)')) stored,
  c_mode               e_telescope_mode_type not null,
  c_program_references text[]                not null default '{}',
  c_partner            e_partner             null,
  c_note               text                  null check (c_note is null or char_length(c_note) > 0),

  constraint mode_interval_order check (c_start < c_end),
  -- The full ProgramReference grammar lives in lucuma-core. This pattern rejects
  -- the shapes that grammar can never accept, which is what keeps a read alive.
  constraint mode_program_references_valid
    check (f_array_matches(c_program_references, '^[A-Z0-9][A-Z0-9-]*$')),
  constraint mode_partner_iff_block_scheduling check ((c_mode = 'BlockScheduling') = (c_partner is not null)),
  constraint mode_no_overlap exclude using gist (c_site with =, c_interval with &&)
);
comment on table t_telescope_mode_block is 'Telescope operating mode over an interval, per site.';

-- The night projection asks for the overlap as `c_interval && tsrange(...)`, which the
-- GiST index behind the exclusion constraint above serves. The flat block queries go
-- through Grackle, whose predicates cannot express `&&`, so they ask for the same overlap
-- as `c_end > $start and c_start < $end`. This btree index serves that form.
create index i_t_telescope_mode_block_site_start on t_telescope_mode_block (c_site, c_start);


-- PUBLISHED SEMESTERS

create type e_moon_phase as enum('New', 'Full');
comment on type e_moon_phase is 'New or full moon.';

create table t_published_semester (
  c_id           bigint     generated always as identity primary key,
  c_site         e_site     not null,
  c_semester     d_semester not null,
  c_title        text       not null check (char_length(c_title) > 0),
  c_version      text       null check (c_version is null or char_length(c_version) > 0),
  c_demo         boolean    not null default false,
  c_nights_start date       not null,
  c_nights_end   date       not null,
  c_holidays     date[]     not null default '{}',

  constraint t_published_semester_unique unique (c_site, c_semester),
  constraint semester_nights_order check (c_nights_start < c_nights_end)
);
comment on table t_published_semester is 'A published semester schedule per site, for the picker.';

create table t_moon_event (
  c_site     e_site       not null,
  c_semester d_semester   not null,
  c_date     date         not null,
  c_phase    e_moon_phase not null,

  constraint t_moon_event_pkey primary key (c_site, c_semester, c_date),
  constraint t_moon_event_fkey foreign key (c_site, c_semester)
    references t_published_semester (c_site, c_semester) on delete cascade
);
comment on table t_moon_event is 'New/full moon dates as printed on a published semester sheet.';

-- INSTRUMENT AVAILABILITY BLOCKS

create type e_resource_usage as enum('SCIENCE', 'ENGINEERING', 'UNAVAILABLE');
comment on type e_resource_usage is 'Operational state of a resource.';

create type e_instrument_place as enum('PORT', 'FLOOR', 'LAB', 'BASE', 'UNKNOWN');
comment on type e_instrument_place is 'Where an instrument physically is.';

create type e_resource_instrument as enum(
  'ACQ_CAM', 'ALOPEKE', 'ALTAIR', 'CAL_ZORRO', 'CANOPUS', 'ENGINEERING',
  'F2', 'GCAL', 'GHOST', 'GMOS', 'GNIRS', 'GPI', 'GSAOI', 'IGRINS2',
  'IQUEYE', 'MAROON_X', 'NIRI', 'SCORPIO', 'UNKNOWN'
);
comment on type e_resource_instrument is 'Everything the published schedules mount or state facts about.';

create table t_instrument_availability_block (
  c_id             bigint                 generated always as identity primary key,
  c_site           e_site                 not null,
  c_start          timestamp              not null,
  c_end            timestamp              not null,
  c_interval       tsrange                not null generated always as (tsrange(c_start, c_end, '[)')) stored,
  c_instrument     e_resource_instrument  not null,
  c_published_name text                   not null check (char_length(c_published_name) > 0),
  c_place          e_instrument_place     not null,
  c_port           integer                null check (c_port > 0),
  c_usage          e_resource_usage       not null,
  c_note           text                   null check (c_note is null or char_length(c_note) > 0),

  constraint instrument_interval_order check (c_start < c_end),
  constraint instrument_port_iff_place check ((c_place = 'PORT') = (c_port is not null)),
  constraint instrument_no_overlap exclude using gist (c_site with =, c_instrument with =, c_interval with &&)
);
comment on table t_instrument_availability_block is 'An instrument''s operational state and location over an interval, per site.';

-- The night projection asks for the overlap as `c_interval && tsrange(...)`, which the
-- GiST index behind the exclusion constraint above serves. The flat block queries go
-- through Grackle, whose predicates cannot express `&&`, so they ask for the same overlap
-- as `c_end > $start and c_start < $end`. This btree index serves that form.
create index i_t_instrument_availability_block_site_start on t_instrument_availability_block (c_site, c_start);

-- TELESCOPE SUBSYSTEM BLOCKS

create type e_telescope_subsystem as enum(
  'PWFS1', 'PWFS2', 'ALTAIR', 'CANOPUS', 'LGS', 'GPOL', 'DOME_SHUTTER', 'DOME_VENT_GATES'
);
comment on type e_telescope_subsystem is 'A telescope subsystem the schedule states facts about.';

create type e_power_source as enum('COMMERCIAL', 'GENERATOR');
comment on type e_power_source is 'What powers a subsystem over a span.';

create table t_telescope_subsystem_block (
  c_id           bigint                generated always as identity primary key,
  c_site         e_site                not null,
  c_start        timestamp             not null,
  c_end          timestamp             not null,
  c_interval     tsrange               not null generated always as (tsrange(c_start, c_end, '[)')) stored,
  c_subsystem    e_telescope_subsystem not null,
  c_usage        e_resource_usage      not null,
  c_power_source e_power_source        null,
  c_note         text                  null check (c_note is null or char_length(c_note) > 0),

  constraint subsystem_interval_order check (c_start < c_end),
  constraint subsystem_no_overlap exclude using gist (c_site with =, c_subsystem with =, c_interval with &&)
);
comment on table t_telescope_subsystem_block is 'A telescope subsystem''s operational state over an interval, per site.';

-- The night projection asks for the overlap as `c_interval && tsrange(...)`, which the
-- GiST index behind the exclusion constraint above serves. The flat block queries go
-- through Grackle, whose predicates cannot express `&&`, so they ask for the same overlap
-- as `c_end > $start and c_start < $end`. This btree index serves that form.
create index i_t_telescope_subsystem_block_site_start on t_telescope_subsystem_block (c_site, c_start);

-- INSTRUMENT COMPONENTS

create type e_instrument_component_type as enum('FILTER', 'DISPERSER', 'FPU', 'WFS', 'OTHER');
comment on type e_instrument_component_type is 'The kind of instrument piece.';

create type e_component_location as enum('INSTALLED', 'FLOOR', 'LAB', 'BASE', 'UNKNOWN');
comment on type e_component_location is 'Where a piece is when it is not installed in its instrument; INSTALLED means "wherever its instrument is".';

create type e_existence as enum('present', 'deleted');
comment on type e_existence is 'Soft-delete state. A DELETED piece stops being offered; its blocks stay valid.';

create sequence s_instrument_component;

create table t_instrument_component (
  c_id             varchar                       primary key default 'c-' || nextval('s_instrument_component'),
  c_instrument     e_resource_instrument         not null,
  c_component_type e_instrument_component_type   not null,
  c_code           text                          not null check (char_length(c_code) > 0),
  c_name           text                          not null check (char_length(c_name) > 0),
  c_barcode        text                          null check (c_barcode is null or char_length(c_barcode) > 0),
  c_aliases        text[]                        not null default '{}',
  c_existence      e_existence                   not null default 'present',

  constraint component_code_unique unique (c_instrument, c_component_type, c_code),
  -- Each alias is read back as a NonEmptyString, so an empty one fails the read.
  constraint component_aliases_nonempty check (f_array_matches(c_aliases, '^[\s\S]+$'))
);
comment on table t_instrument_component is 'The instrument component catalog. Identity carries no site.';

create table t_instrument_component_block (
  c_id           bigint               generated always as identity primary key,
  c_site         e_site               not null,
  c_start        timestamp            not null,
  c_end          timestamp            not null,
  c_interval     tsrange              not null generated always as (tsrange(c_start, c_end, '[)')) stored,
  c_component_id varchar              not null references t_instrument_component (c_id) on delete cascade,
  c_usage        e_resource_usage     not null,
  c_location     e_component_location not null,
  c_note         text                 null check (c_note is null or char_length(c_note) > 0),

  constraint component_interval_order check (c_start < c_end),
  -- The subject is the physical piece: one piece cannot be in two states
  -- or two places at once, so the constraint carries no site column.
  constraint component_no_overlap exclude using gist (c_component_id with =, c_interval with &&)
);
comment on table t_instrument_component_block is 'A span of a component''s life: where it was and whether it was usable.';

-- The night projection asks for the overlap as `c_interval && tsrange(...)`, which the
-- GiST index behind the exclusion constraint above serves. The flat block queries go
-- through Grackle, whose predicates cannot express `&&`, so they ask for the same overlap
-- as `c_end > $start and c_start < $end`. This btree index serves that form.
create index i_t_instrument_component_block_site_start on t_instrument_component_block (c_site, c_start);

-- This table is the one whose exclusion constraint carries no site column, so the
-- constraint's GiST index cannot serve the night projection's site + overlap query.
create index i_t_instrument_component_block_site_interval on t_instrument_component_block using gist (c_site, c_interval);

-- c_component_id backs an `on delete cascade` foreign key. The exclusion constraint
-- leads with the same column, but a btree serves the cascade's equality lookup better.
create index i_t_instrument_component_block_component on t_instrument_component_block (c_component_id);

-- The component catalog a site's records cover: every component with at least one
-- block at that site. c_search holds the text the `components` query searches, so
-- that query can use a single case-insensitive LIKE. Grackle has no array predicate,
-- so the aliases have to be flattened into text here.
create view v_instrument_component_at_site as
select distinct
  b.c_site,
  c.c_id,
  c.c_instrument,
  c.c_component_type,
  c.c_code,
  c.c_name,
  c.c_barcode,
  c.c_aliases,
  c.c_existence,
  lower(
    c.c_name || ' ' || c.c_code || ' ' ||
    coalesce(c.c_barcode, '') || ' ' ||
    array_to_string(c.c_aliases, ' ')
  ) as c_search
from t_instrument_component c
join t_instrument_component_block b on b.c_component_id = c.c_id;
comment on view v_instrument_component_at_site is 'The component catalog per site, with a flattened search column.';
