

create table t_seeing_trend (
  c_tag   d_tag    not null primary key,
  c_name  varchar  not null
);

insert into t_seeing_trend values ('getting_better', 'Getting Better');
insert into t_seeing_trend values ('staying_the_same', 'Staying the Same');
insert into t_seeing_trend values ('getting_worse', 'Getting Worse');
insert into t_seeing_trend values ('variable', 'Variable');

create table t_conditions_expectation_type (
  c_tag   d_tag    not null primary key,
  c_name  varchar  not null
);

insert into t_conditions_expectation_type values ('thin_clouds', 'Thin Clouds');
insert into t_conditions_expectation_type values ('thick_clouds', 'Thick Clouds');
insert into t_conditions_expectation_type values ('fog', 'Fog');
insert into t_conditions_expectation_type values ('clear_skies', 'Clear Skies');

create table t_conditions_source (
  c_tag   d_tag    not null primary key,
  c_name  varchar  not null
);

insert into t_conditions_source values ('observer', 'Observer');

-- this table is insert-only
create table t_chron_conditions_entry (

  c_chron_id        d_chron_id  not null primary key default nextval('s_chron_id'),
  c_timestamp       timestamptz not null default current_timestamp,
  c_user            d_user_id   null references t_user(c_user_id) default current_setting('lucuma.user', true),
  c_transaction_id  xid8        not null default pg_current_xact_id(),

  c_measurement_source         d_tag references t_conditions_source (c_tag),
  c_measurement_seeing         d_angle_µas,
  c_measurement_extinction_pct d_int_percentage,
  c_measurement_wavelength     d_wavelength_pm,
  c_measurement_azimuth        d_angle_µas,
  c_measurement_elevation      d_angle_µas,

  c_intuition_seeing_trend     d_tag references t_seeing_trend (c_tag),
  c_intuition_expectation      d_tag references t_conditions_expectation_type (c_tag),
  c_intuition_timespan         interval(6)

  -- TODO: check constraints

);


create view v_chron_conditions_entry as
  select *,
  case when num_nulls(c_measurement_source, c_measurement_seeing, c_measurement_extinction_pct, c_measurement_wavelength, c_measurement_azimuth, c_measurement_elevation) < 6 then c_chron_id end as c_measurement_id,
  case when num_nulls(c_intuition_expectation, c_intuition_timespan, c_intuition_seeing_trend) < 3 then c_chron_id end as c_intuition_id,
  case when num_nulls(c_intuition_expectation, c_intuition_timespan) < 2 then c_chron_id end as c_expectation_id,
  case when c_measurement_wavelength is not null then c_chron_id end as c_wavelength_id
  from t_chron_conditions_entry;

