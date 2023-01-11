
-- add more nullable IDs for embedded objects
create or replace view v_observation as
  select *,
  case when c_explicit_ra              is not null then c_observation_id end as c_explicit_base_id,
  case when c_air_mass_min             is not null then c_observation_id end as c_air_mass_id,
  case when c_hour_angle_min           is not null then c_observation_id end as c_hour_angle_id,
  case when c_observing_mode_type      is not null then c_observation_id end as c_observing_mode_id,
  case when c_spec_wavelength          is not null then c_observation_id end as c_spec_wavelength_id,          -- this is new
  case when c_spec_signal_to_noise_at  is not null then c_observation_id end as c_spec_signal_to_noise_at_id,  -- this is new
  case when c_spec_wavelength_coverage is not null then c_observation_id end as c_spec_wavelength_coverage_id, -- this is new
  case when c_spec_focal_plane_angle   is not null then c_observation_id end as c_spec_focal_plane_angle_id    -- this is new
  from t_observation;
