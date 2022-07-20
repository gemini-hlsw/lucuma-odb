--- AIR MASS

create domain d_air_mass as numeric(3,2)
check (value between 1.0 AND 3.0);
comment on domain d_air_mass is 'air mass limited to the range [1.00, 3.00]';

--- HOUR ANGLE

create domain d_hour_angle as numeric(3,2)
check (value between -5.0 and 5.0);
comment on domain d_hour_angle is 'hour angle limited to the range [-5.0, 5.0]';


--- CLOUD EXTINCTION

create table t_cloud_extinction (
  c_tag        d_tag         primary key,
  c_short_name varchar       not null,
  c_long_name  varchar       not null,
  c_value      numeric(2,1)  not null check(c_value > 0.0 and c_value <= 3.0)
);

insert into t_cloud_extinction values ('ZeroPointOne',   '0.1', '0.1', 0.1);
insert into t_cloud_extinction values ('ZeroPointThree', '0.3', '0.3', 0.3);
insert into t_cloud_extinction values ('ZeroPointFive',  '0.5', '0.5', 0.5);
insert into t_cloud_extinction values ('OnePointZero',   '1.0', '1.0', 1.0);
insert into t_cloud_extinction values ('OnePointFive',   '1.5', '1.5', 1.5);
insert into t_cloud_extinction values ('TwoPointZero',   '2.0', '2.0', 2.0);
insert into t_cloud_extinction values ('ThreePointZero', '3.0', '3.0', 3.0);

--- IMAGE QUALITY

create table t_image_quality (
  c_tag        d_tag         primary key,
  c_short_name varchar       not null,
  c_long_name  varchar       not null,
  c_value      numeric(2,1)  not null check(c_value > 0.0 and c_value <= 2.0)
);

insert into t_image_quality values ('ZeroPointOne',   '0.1', '0.1', 0.1);
insert into t_image_quality values ('ZeroPointTwo',   '0.2', '0.2', 0.2);
insert into t_image_quality values ('ZeroPointThree', '0.3', '0.3', 0.3);
insert into t_image_quality values ('ZeroPointFour',  '0.4', '0.4', 0.4);
insert into t_image_quality values ('ZeroPointSix',   '0.6', '0.6', 0.6);
insert into t_image_quality values ('ZeroPointEight', '0.8', '0.8', 0.8);
insert into t_image_quality values ('OnePointZero',   '1.0', '1.0', 1.0);
insert into t_image_quality values ('OnePointFive',   '1.5', '1.5', 1.5);
insert into t_image_quality values ('TwoPointZero',   '2.0', '2.0', 2.0);

--- SKY BACKGROUND

create table t_sky_background (
  c_tag        d_tag         primary key,
  c_short_name varchar       not null,
  c_long_name  varchar       not null
);

insert into t_sky_background values ('Darkest', 'Darkest', 'Darkest');
insert into t_sky_background values ('Dark', 'Dark', 'Dark');
insert into t_sky_background values ('Gray', 'Gray', 'Gray');
insert into t_sky_background values ('Bright', 'Bright', 'Bright');

--- WATER VAPOR

create table t_water_vapor (
  c_tag        d_tag         primary key,
  c_short_name varchar       not null,
  c_long_name  varchar       not null
);

insert into t_water_vapor values ('VeryDry', 'Very Dry', 'Very Dry');
insert into t_water_vapor values ('Dry',     'Dry',      'Dry'     );
insert into t_water_vapor values ('Median',  'Median',   'Median'  );
insert into t_water_vapor values ('Wet',     'Wet',      'Wet'     );

--- CONSTRAINT SET

create table t_constraint_set (
  c_observation_id   d_observation_id primary key references t_observation(c_observation_id) on delete cascade,
  c_cloud_extinction d_tag            not null    references t_cloud_extinction(c_tag),
  c_image_quality    d_tag            not null    references t_image_quality(c_tag),
  c_sky_background   d_tag            not null    references t_sky_background(c_tag),
  c_water_vapor      d_tag            not null    references t_water_vapor(c_tag),
  c_air_mass_min     d_air_mass       null default 1.0,
  c_air_mass_max     d_air_mass       null default 2.0,
  c_hour_angle_min   d_hour_angle     null default null,
  c_hour_angle_max   d_hour_angle     null default null,

  -- both air mass fields are defined or neither are defined
  constraint air_mass_neither_or_both
  check (num_nulls(c_air_mass_min, c_air_mass_max) <> 1),

  -- both hour angle fields are defined or neither are defined
  constraint hour_angle_neither_or_both
  check (num_nulls(c_hour_angle_min, c_hour_angle_max) <> 1),

  -- one elevation range definition is required, but both cannot be defined
  constraint one_elevation_range
  check (num_nulls(c_air_mass_min, c_air_mass_max, c_hour_angle_min, c_hour_angle_max) = 2)
);
comment on table t_constraint_set is 'Constraint sets, 1:1 with Observation.';