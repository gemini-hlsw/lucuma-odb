-- Execution timestamps: when an observation's execution began, when its last
-- event arrived, and when it completed.  Backs `Execution.timestamps`.

-- The observation's computed execution state, formerly an inline CASE in
-- v_generator_params.  Pulled out so that v_execution_timestamps and
-- `Execution.executionState` share the logic.  A plain view rather than a
-- function: the planner flattens a view into the query that uses it, whereas a
-- SQL function whose body has sub-SELECTs is never inlined.
CREATE VIEW v_execution_state AS
SELECT
  o.c_observation_id,
  CASE
    -- The observation has a declared state.
    WHEN o.c_declared_state IS NOT NULL THEN o.c_declared_state

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
  END AS c_execution_state
FROM t_observation o;

-- Same view, with the CASE replaced by a join to v_execution_state.  Body
-- otherwise copied from V1347.
CREATE OR REPLACE VIEW v_generator_params AS
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
  o.c_declared_state,
  s.c_execution_state,
  COALESCE(s_counts.c_step_count, 0) AS c_step_count,
  o.c_scheduling_mode,
  o.c_blind_offset_target_id,
  b.c_sid_rv AS c_blind_rv,
  b.c_source_profile AS c_blind_source_profile,
  t.c_target_id,
  t.c_sid_rv,
  t.c_source_profile,
  COALESCE(t.c_is_signal_to_noise_target, false) AS c_is_signal_to_noise_target,
  o.c_altair_mode,
  o.c_altair_field_lens,
  o.c_altair_cass_rotator,
  o.c_altair_nd_filter,
  o.c_explicit_guide_probe,
  COALESCE(t.c_type = 'nonsidereal'::e_target_type, false) AS c_is_nonsidereal,
  o.c_is_user_defined_telluric
FROM
  t_observation o
JOIN v_execution_state s ON s.c_observation_id = o.c_observation_id
LEFT JOIN t_target b ON b.c_target_id = o.c_blind_offset_target_id
LEFT JOIN LATERAL (
  SELECT t.c_target_id,
         t.c_type,
         t.c_sid_rv,
         t.c_source_profile,
         a.c_is_signal_to_noise_target
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

-- Nothing indexed visits by observation, yet `Execution.visits` and the view
-- below both look them up that way.  The visit times are deliberately left out
-- of the index: update_visit_times rewrites c_end on every execution event,
-- and indexing it would turn each of those updates from HOT into a full one.
CREATE INDEX i_visit_observation
  ON t_visit (c_observation_id);

-- One row per observation.  The first and last event times are the earliest
-- and latest event times across all of its visits (t_visit.c_start / c_end,
-- kept up to date by update_visit_times), so slews count.  The completion time
-- is the last event time once the observation is completed or declared
-- complete; declaring an observation complete does not itself set a time, so a
-- declared-complete observation without events has none.
CREATE VIEW v_execution_timestamps AS
SELECT
  s.c_observation_id,
  v.c_first_event_time,
  v.c_last_event_time,
  CASE
    WHEN v.c_last_event_time IS NULL THEN NULL
    WHEN s.c_execution_state IN ('completed'::e_execution_state, 'declared_complete'::e_execution_state)
      THEN v.c_last_event_time
  END AS c_completion_time
FROM v_execution_state s
LEFT JOIN LATERAL (
  SELECT
    MIN(t.c_start) AS c_first_event_time,
    MAX(t.c_end)   AS c_last_event_time
  FROM t_visit t
  WHERE t.c_observation_id = s.c_observation_id
) v ON TRUE;
