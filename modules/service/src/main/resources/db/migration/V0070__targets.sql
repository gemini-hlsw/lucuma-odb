

create DOMAIN d_target_id as varchar; -- TODO format check
comment on domain d_target_id is 'GID type for targets.';

create type e_target_type as enum('sidereal', 'nonsidereal');
create type e_catalog_name as enum('simbad', 'gaia');
create type e_ephemeris_key_type as enum('comet', 'asteroid_new', 'asteroid_old', 'major_body', 'user_supplied');

-- Reference observation epoch in format '[JB]YYYY.YYY'
create domain d_epoch as varchar check(value similar to '[JB]\d{4}\.\d{3}');

create sequence s_target_id start with 256; -- three hex digits
create table t_target (

  -- primary properties
  c_program_id  d_program_id    not null references t_program(c_program_id),
  c_target_id   d_target_id     not null primary key default 't-' || to_hex(nextval('s_target_id')),
  c_name        text            check (c_name is null or length(c_name) > 0),
  c_existence   e_existence     not null default 'present',

  -- tracking type
  c_type        e_target_type   not null,

  -- either it's sidereal or all the sidereal fields must be null
  check (c_type = 'sidereal' or
    num_nulls(
      c_sid_ra,
      c_sid_dec,
      c_sid_epoch,
      c_sid_pm_ra,
      c_sid_pm_dec,
      c_sid_rv,
      c_sid_parallax,
      c_sid_catalog_name,
      c_sid_catalog_id,
      c_sid_catalog_object_type
    ) = 10),

  -- either it's nonsidereal or all the nonsidereal fields must be null
  check (c_type = 'nonsidereal' or
    num_nulls(
      c_nsid_des,
      c_nsid_key_type,
      c_nsid_key
    ) = 3),

  -- sidereal
  c_sid_ra      d_angle_µas     null default null,
  c_sid_dec     d_angle_µas     null default null,

  -- both null or both defined
  check (num_nulls(c_sid_pm_ra, c_sid_pm_dec) <> 1),

  c_sid_epoch   d_epoch         null default null,

  -- proper motion (both defined or both null)
  c_sid_pm_ra   d_angle_µas     null default null,
  c_sid_pm_dec  d_angle_µas     null default null,

  -- radial velocity
  c_sid_rv numeric, -- TODO  -- km/sec

  -- parallax -- µas
  c_sid_parallax d_angle_µas    null default null,

  -- catalog info
  c_sid_catalog_name        e_catalog_name null default null,
  c_sid_catalog_id          varchar        null default null,
  c_sid_catalog_object_type varchar        null default null,

  -- both defined or both null
  check (num_nulls(c_sid_catalog_name, c_sid_catalog_id) != 1),

  -- nonsidereal
  c_nsid_des        varchar null default null,
  c_nsid_key_type   e_ephemeris_key_type null default null,
  c_nsid_key        varchar null default null,

  -- required for nonsidereal targets
  check (c_type = 'sidereal' or num_nulls(c_nsid_des, c_nsid_key_type, c_nsid_key) = 0),

  -- source profile is just a blob. we'll see how this works
  c_source_profile jsonb not null

);


-- a view that has synthetic nullable ids for nullable embedded objects (required by grackle)
create view v_target as
  select *,
  case when c_type='sidereal'              then c_target_id end as c_sidereal_id,
  case when c_type='nonsidereal'           then c_target_id end as c_nonsidereal_id,
  case when c_sid_catalog_name is not null then c_target_id end as c_sid_catalog_info_id,
  case when c_sid_pm_ra        is not null then c_target_id end as c_sid_pm_id,
  case when c_sid_parallax     is not null then c_target_id end as c_sid_parallax_id,
  case when c_sid_rv           is not null then c_target_id end as c_sid_rv_id
  from t_target;


