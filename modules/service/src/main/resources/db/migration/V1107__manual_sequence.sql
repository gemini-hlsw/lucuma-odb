DROP VIEW v_generator_params;

-- Recreate generator params with the new execution state and atom count.
CREATE VIEW v_generator_params AS
SELECT
  o.c_program_id,
  o.c_observation_id,
  o.c_calibration_role,
  o.c_image_quality,
  o.c_cloud_extinction,
  o.c_sky_background,
  o.c_water_vapor,
  o.c_air_mass_min,
  o.c_air_mass_max,
  o.c_hour_angle_min,
  o.c_hour_angle_max,
  e.c_exposure_time_mode,
  e.c_signal_to_noise,
  e.c_signal_to_noise_at,
  e.c_exposure_time,
  e.c_exposure_count,
  o.c_observing_mode_type,
  o.c_science_band,
  o.c_declared_complete,
  CASE
    -- The observation is explicitly marked complete -> completed.
    WHEN o.c_declared_complete
      THEN 'completed'::e_execution_state

    -- No events have been fired at all -> not_started (just slewing to the
    -- target doesn't count as execution).
    WHEN NOT EXISTS (
      SELECT 1
      FROM   t_execution_event v
      WHERE  v.c_observation_id = o.c_observation_id
        AND  v.c_event_type != 'slew'::e_execution_event_type
    ) THEN 'not_started'::e_execution_state

    -- At least one step not completed -> ongoing
    WHEN EXISTS (
      SELECT 1
      FROM t_step s
      JOIN t_atom a ON a.c_atom_id = s.c_atom_id AND a.c_observation_id = o.c_observation_id AND a.c_sequence_type = 'science'
      LEFT JOIN t_step_execution se       ON se.c_step_id = s.c_step_id
      LEFT JOIN t_step_execution_state es ON es.c_tag     = se.c_execution_state AND es.c_terminal
      WHERE es.c_tag IS NULL -- no step execution or a non-terminal execution state
    ) THEN 'ongoing'::e_execution_state

    ELSE 'completed'::e_execution_state
  END AS c_execution_state,
  COALESCE(s_counts.c_step_count, 0) AS c_step_count,
  o.c_blind_offset_target_id,
  b.c_sid_rv AS c_blind_rv,
  b.c_source_profile AS c_blind_source_profile,
  t.c_target_id,
  t.c_sid_rv,
  t.c_source_profile
FROM
  t_observation o
LEFT JOIN t_target b ON b.c_target_id = o.c_blind_offset_target_id
LEFT JOIN LATERAL (
  SELECT t.c_target_id,
         t.c_sid_rv,
         t.c_source_profile
    FROM t_asterism_target a
    INNER JOIN t_target t
      ON a.c_target_id = t.c_target_id
     AND t.c_existence = 'present'
   WHERE a.c_observation_id = o.c_observation_id
) t ON TRUE
LEFT JOIN t_exposure_time_mode e
  ON e.c_observation_id = o.c_observation_id
 AND e.c_role = 'requirement'
LEFT JOIN (
  SELECT
    se.c_observation_id,
    COUNT(*) AS c_step_count
  FROM t_step_execution se
  GROUP BY se.c_observation_id
) s_counts ON s_counts.c_observation_id = o.c_observation_id
ORDER BY
  o.c_observation_id,
  t.c_target_id;