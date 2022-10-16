create type e_pac_mode as enum('unbounded', 'fixed', 'allow_flip', 'average_parallactic', 'parallactic_override');

--- OBSERVING CONDITIONS: AIR MASS

create domain d_air_mass as numeric(3,2)
check (value between 1.0 AND 3.0);
comment on domain d_air_mass is 'air mass limited to the range [1.00, 3.00]';

--- OBSERVING CONDITIONS: HOUR ANGLE

create domain d_hour_angle as numeric(3,2)
check (value between -5.0 and 5.0);
comment on domain d_hour_angle is 'hour angle limited to the range [-5.0, 5.0]';


--- OBSERVING CONDITIONS: CLOUD EXTINCTION

create table t_cloud_extinction (
  c_tag        d_tag         primary key,
  c_short_name varchar       not null,
  c_long_name  varchar       not null,
  c_value      numeric(2,1)  not null check(c_value > 0.0 and c_value <= 3.0)
);

insert into t_cloud_extinction values ('point_one',   '0.1', '0.1', 0.1);
insert into t_cloud_extinction values ('point_three', '0.3', '0.3', 0.3);
insert into t_cloud_extinction values ('point_five',  '0.5', '0.5', 0.5);
insert into t_cloud_extinction values ('one_point_zero',   '1.0', '1.0', 1.0);
insert into t_cloud_extinction values ('one_point_five',   '1.5', '1.5', 1.5);
insert into t_cloud_extinction values ('two_point_zero',   '2.0', '2.0', 2.0);
insert into t_cloud_extinction values ('three_point_zero', '3.0', '3.0', 3.0);

--- OBSERVING CONDITIONS: IMAGE QUALITY

create table t_image_quality (
  c_tag        d_tag         primary key,
  c_short_name varchar       not null,
  c_long_name  varchar       not null,
  c_value      numeric(2,1)  not null check(c_value > 0.0 and c_value <= 2.0)
);

insert into t_image_quality values ('point_one',   '0.1', '0.1', 0.1);
insert into t_image_quality values ('point_two',   '0.2', '0.2', 0.2);
insert into t_image_quality values ('point_three', '0.3', '0.3', 0.3);
insert into t_image_quality values ('point_four',  '0.4', '0.4', 0.4);
insert into t_image_quality values ('point_six',   '0.6', '0.6', 0.6);
insert into t_image_quality values ('point_eight', '0.8', '0.8', 0.8);
insert into t_image_quality values ('one_point_zero',   '1.0', '1.0', 1.0);
insert into t_image_quality values ('one_point_five',   '1.5', '1.5', 1.5);
insert into t_image_quality values ('two_point_zero',   '2.0', '2.0', 2.0);

--- OBSERVING CONDITIONS: SKY BACKGROUND

create table t_sky_background (
  c_tag        d_tag         primary key,
  c_short_name varchar       not null,
  c_long_name  varchar       not null
);

insert into t_sky_background values ('darkest', 'Darkest', 'Darkest');
insert into t_sky_background values ('dark', 'Dark', 'Dark');
insert into t_sky_background values ('gray', 'Gray', 'Gray');
insert into t_sky_background values ('bright', 'Bright', 'Bright');

--- OBSERVING CONDITIONS: WATER VAPOR

create table t_water_vapor (
  c_tag        d_tag         primary key,
  c_short_name varchar       not null,
  c_long_name  varchar       not null
);

insert into t_water_vapor values ('very_dry', 'Very Dry', 'Very Dry');
insert into t_water_vapor values ('dry',     'Dry',      'Dry'     );
insert into t_water_vapor values ('median',  'Median',   'Median'  );
insert into t_water_vapor values ('wet',     'Wet',      'Wet'     );

-- OBSERVATION STATUS

create type e_obs_status as enum(
  'new',
  'included',
  'proposed',
  'approved',
  'for_review',
  'ready',
  'ongoing',
  'observed'
);

create type e_obs_active_status as enum('active', 'inactive');

-- SCIENCE REQUIREMENTS: MODE

create table t_science_mode (
  c_tag        d_tag         primary key,
  c_short_name varchar       not null,
  c_long_name  varchar       not null
);

insert into t_science_mode values ('imaging', 'Imaging', 'Imaging');
insert into t_science_mode values ('spectroscopy', 'Spectroscopy', 'Spectroscopy');


-- SCIENCE REQUIREMENTS: SPECTROSCOPY FOCAL PLANE

create table t_focal_plane (
  c_tag        d_tag         primary key,
  c_short_name varchar       not null,
  c_long_name  varchar       not null
);

insert into t_focal_plane values ('single_slit', 'Single Slit', 'Single Slit');
insert into t_focal_plane values ('multiple_slit', 'Multiple Slit', 'Multiple Slit');
insert into t_focal_plane values ('ifu', 'IFU', 'Integral Field Unit');

-- SCIENCE REQUIREMENTS: SPECTROSCOPY CAPABILITIES

create table t_spectroscopy_capabilities (
  c_tag        d_tag         primary key,
  c_short_name varchar       not null,
  c_long_name  varchar       not null
);

insert into t_spectroscopy_capabilities values ('nod_and_shuffle', 'Nod and Shuffle', 'Nod and Shuffle');
insert into t_spectroscopy_capabilities values ('polarimetry', 'Polarimetry', 'Polarimetry');
insert into t_spectroscopy_capabilities values ('coronagraphy', 'Coronagraphy', 'Coronagraphy');

-- OBSERVING MODE

create type e_observing_mode_type as enum(
  'gmos_north_long_slit',
  'gmos_south_long_slit'
);

-- OBSERVATIONS

create domain d_observation_id as varchar; -- TODO format check
comment on domain d_observation_id is 'GID type for observations.';

create sequence s_observation_id START with 256; -- three hex digits
create table t_observation (
  c_program_id         d_program_id        not null    references t_program(c_program_id),
  c_observation_id     d_observation_id    primary key default 'o-' || to_hex(nextval('s_observation_id')),
  c_existence          e_existence         not null    default 'present',
  c_subtitle           text                null        check (length(c_subtitle) > 0),
  c_instrument         d_tag               null        references t_instrument(c_tag),
  c_status             e_obs_status        not null    default 'new',
  c_active_status      e_obs_active_status not null    default 'active',
  c_visualization_time timestamp           null        default null,

  -- position angle constraint
  c_pac_mode           e_pac_mode          not null    default 'unbounded',
  c_pac_angle          d_angle_µas         not null    default 0,

  -- target environment
  c_explicit_ra        d_angle_µas         null        default null,
  c_explicit_dec       d_angle_µas         null        default null,

  -- both explicit coordinates are defined or neither are defined
  constraint explicit_base_neither_or_both
  check (num_nulls(c_explicit_ra, c_explicit_dec) <> 1),

  -- observing conditions (aka "observing constraints")
  c_cloud_extinction   d_tag               not null    references t_cloud_extinction(c_tag),
  c_image_quality      d_tag               not null    references t_image_quality(c_tag),
  c_sky_background     d_tag               not null    references t_sky_background(c_tag),
  c_water_vapor        d_tag               not null    references t_water_vapor(c_tag),
  c_air_mass_min       d_air_mass          null        default 1.0,
  c_air_mass_max       d_air_mass          null        default 2.0,
  c_hour_angle_min     d_hour_angle        null        default null,
  c_hour_angle_max     d_hour_angle        null        default null,

  -- observing conditions: both air mass fields are defined or neither are defined
  constraint air_mass_neither_or_both
  check (num_nulls(c_air_mass_min, c_air_mass_max) <> 1),

  -- observing conditions: both hour angle fields are defined or neither are defined
  constraint hour_angle_neither_or_both
  check (num_nulls(c_hour_angle_min, c_hour_angle_max) <> 1),

  -- observing conditions: one elevation range definition is required, but both cannot be defined
  constraint one_elevation_range
  check (num_nulls(c_air_mass_min, c_air_mass_max, c_hour_angle_min, c_hour_angle_max) = 2),

  -- science requirements
  c_science_mode            d_tag           not null default 'spectroscopy' references t_science_mode(c_tag),

  -- imaging science requirements (TBD)

  -- spectroscopy science requirements
  c_spec_wavelength          d_wavelength_pm null default null,
  c_spec_resolution          integer         null default null,
  c_spec_signal_to_noise     numeric(5,2)    null default null, -- TODO, what's an appropriate precision and scale?
  c_spec_signal_to_noise_at  d_wavelength_pm null default null,
  c_spec_wavelength_coverage d_wavelength_pm null default null,
  c_spec_focal_plane         d_tag           null default null references t_focal_plane(c_tag),
  c_spec_focal_plane_angle   d_angle_µas     null default null,
  c_spec_capability          d_tag           null default null references t_spectroscopy_capabilities(c_tag),

  -- spectroscopy: requested resolution is positive
  constraint spectroscopy_resolution_positive
  check (c_spec_resolution > 0),

  -- spectroscopy: S/N is positive
  constraint spectroscopy_signal_to_noise_positive
  check (c_spec_signal_to_noise > 0),

  -- observing mode
  c_observing_mode_type e_observing_mode_type null default null,

  --

  unique (c_observation_id, c_instrument),
  unique (c_observation_id, c_observing_mode_type),
  unique (c_program_id, c_observation_id)
);
comment on table t_observation is 'Observations.';

create view v_observation as
  select *,
  case when c_explicit_ra    is not null then c_observation_id end as c_explicit_base_id,
  case when c_air_mass_min   is not null then c_observation_id end as c_air_mass_id,
  case when c_hour_angle_min is not null then c_observation_id end as c_hour_angle_id
  from t_observation