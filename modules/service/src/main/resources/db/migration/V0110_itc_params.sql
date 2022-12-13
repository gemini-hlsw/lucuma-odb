CREATE VIEW v_itc_params AS
SELECT
  o.c_observation_id,
  o.c_image_quality,
  o.c_cloud_extinction,
  o.c_sky_background,
  o.c_water_vapor,
  o.c_air_mass_min,
  o.c_air_mass_max,
  o.c_hour_angle_min,
  o.c_hour_angle_max,
  o.c_spec_signal_to_noise,
  o.c_spec_signal_to_noise_at,
  o.c_observing_mode_type,
  t.c_target_id,
  t.c_sid_rv,
  t.c_source_profile
FROM
  t_observation o
INNER JOIN t_asterism_target a
  ON o.c_observation_id = a.c_observation_id
INNER JOIN t_target t
  ON a.c_target_id      = t.c_target_id
WHERE
      o.c_spec_signal_to_noise IS NOT NULL
  AND o.c_observing_mode_type  IS NOT NULL
  AND t.c_sid_rv               IS NOT NULL
ORDER BY
  o.c_observation_id,
  t.c_target_id
