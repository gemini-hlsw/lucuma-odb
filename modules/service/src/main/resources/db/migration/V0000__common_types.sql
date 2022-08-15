
--- TAGS

create domain d_tag as varchar;
comment on domain d_tag is 'Tag for a value in a lookup table.';

--- ANGLES

create domain d_angle_µas as bigint;
comment on domain d_angle_µas is 'Angles in integral microarcseconds.';

--- EXISTENCE

create type e_existence as enum ('present', 'deleted');
comment on type e_existence is 'Logical deletion indicator.';

--- EVENTS

create sequence s_event_id cycle;

--- FILTER TYPES

create table t_filter_type (
  c_tag               d_tag       not null primary key,
  c_short_name        varchar     not null,
  c_long_name 		    varchar     not null
);

insert into t_filter_type values ('BroadBand', 'Broad-Band', 'Broad-Band Filter');
insert into t_filter_type values ('NarrowBand', 'Narrow-Band', 'Narrow-Band Filter');
insert into t_filter_type values ('Combination', 'Combination', 'Combination Filter');
insert into t_filter_type values ('Spectroscopic', 'Spectroscopic', 'Spectroscopic Filter');
insert into t_filter_type values ('Engineering', 'Engineering', 'Engineering Filter');

--- OFFSETS

create type d_offset as (
  p d_angle_µas,
  q d_angle_µas
);
comment on type d_offset is 'Composite type for offsets, a pair of angles.';

--- PIXELS

create domain d_pixels as int check (VALUE >= 0);
comment on domain d_pixels is 'Pixel count.';

--- WAVELENGTHS

create domain d_wavelength_pm as int check(VALUE > 0);

--- WAVELENGTH RANGES

create domain d_wavelength_pm_range as int4range;

-- SITES

create type e_site as enum('gn', 'gs');
