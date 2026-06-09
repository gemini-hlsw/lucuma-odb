-- SITES

create type e_site as enum('gn', 'gs');
comment on type e_site is 'Observatory site.';

-- TELESCOPE AVAILABILITY

create type e_telescope_availability as enum('Open', 'Closed');
comment on type e_telescope_availability is 'Whether the telescope is open or closed.';

-- TOO SUPPORT

create type e_too_support as enum('Standard', 'Interrupt', 'Rapid', 'None');
comment on type e_too_support is 'Level of Target of Opportunity support.';

-- TELESCOPE MODE TYPE

create type e_telescope_mode_type as enum('Queue', 'Classical', 'PriorityVisitor', 'Engineering', 'Commissioning');
comment on type e_telescope_mode_type is 'Telescope observing mode type.';

-- TELESCOPE NIGHT TIMELINE

create table t_telescope_night_timeline (
  c_site                    e_site    not null,
  c_observing_night         date      not null,
  c_display_interval_start  timestamp not null,
  c_display_interval_end    timestamp not null,

  constraint t_telescope_night_timeline_pkey primary key (c_site, c_observing_night),

  constraint display_interval_order check (c_display_interval_start < c_display_interval_end)
);
comment on table t_telescope_night_timeline is 'Telescope night timeline per site and observing night.';

-- TELESCOPE AVAILABILITY STATUS

create table t_telescope_availability_status (
  c_site                      e_site                    not null,
  c_observing_night           date                      not null,
  c_interval_start            timestamp                 not null,
  c_interval_end              timestamp                 not null,
  c_availability              e_telescope_availability  not null,
  c_reason                    text                      null,
  c_planned_availability      e_telescope_availability  null,

  constraint t_telescope_availability_status_pkey primary key (c_site, c_observing_night, c_interval_start),

  constraint t_telescope_availability_status_fkey
    foreign key (c_site, c_observing_night)
    references t_telescope_night_timeline(c_site, c_observing_night)
    on delete cascade,

  constraint availability_interval_order check (c_interval_start < c_interval_end)
);
comment on table t_telescope_availability_status is 'Telescope availability status intervals within a night.';

-- TELESCOPE TOO STATUS

create table t_telescope_too_status (
  c_site             e_site        not null,
  c_observing_night  date          not null,
  c_interval_start   timestamp     not null,
  c_interval_end     timestamp     not null,
  c_too_support      e_too_support not null,

  constraint t_telescope_too_status_pkey primary key (c_site, c_observing_night, c_interval_start),

  constraint t_telescope_too_status_fkey
    foreign key (c_site, c_observing_night)
    references t_telescope_night_timeline(c_site, c_observing_night)
    on delete cascade,

  constraint too_interval_order check (c_interval_start < c_interval_end)
);
comment on table t_telescope_too_status is 'Target of Opportunity support level intervals within a night.';

-- TELESCOPE MODE STATUS

create table t_telescope_mode_status (
  c_site                   e_site                not null,
  c_observing_night        date                  not null,
  c_interval_start         timestamp             not null,
  c_interval_end           timestamp             not null,
  c_mode_type              e_telescope_mode_type not null,
  c_mode_program_reference text                  null,

  constraint t_telescope_mode_status_pkey primary key (c_site, c_observing_night, c_interval_start),

  constraint t_telescope_mode_status_fkey
    foreign key (c_site, c_observing_night)
    references t_telescope_night_timeline(c_site, c_observing_night)
    on delete cascade,

  constraint mode_interval_order check (c_interval_start < c_interval_end)
);
comment on table t_telescope_mode_status is 'Telescope observing mode intervals within a night.';
