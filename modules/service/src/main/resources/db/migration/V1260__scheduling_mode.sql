-- Part 2 of 2 (see V1259).  Renames the execution requirement to the scheduling
-- mode and makes it the single observation-level statement of what the Scheduler
-- may do:
--
--   unconstrained    the Scheduler may do as it likes
--   no_splitting     must be executed as a single visit
--   uninterruptible  the above, and no ToO may interrupt it
--   interrupting     the above, and this observation may interrupt another
--
-- WHAT GOES AWAY, AND WHY
--
-- V1245 made the execution requirement an explicit/default/effective triple in
-- which the default was a *floor*: a rapid or interrupting ToO activation raised
-- the effective requirement to 'uninterruptible' regardless of what was declared.
-- That existed to simulate a dependency between two fields that were never
-- really independent.
--
-- With the fourth rung in place, the ladder expresses the dependency structurally
-- -- 'interrupting' contains 'uninterruptible' contains 'no_splitting' -- so the
-- floor has nothing left to do.  The direction of derivation reverses: the ToO
-- activation becomes a function of the scheduling mode rather than the mode being
-- floored by the activation.  execution_requirement_default and
-- execution_requirement_effective are therefore dropped outright, and the
-- observation carries one plain NOT NULL column again.
--
-- Deriving c_too_activation from the mode and the asterism is the next migration;
-- until it lands the activation is still PI-declared.

-------------------------------------------------------------------------------
-- Rename, and restore the plain NOT NULL column.
-------------------------------------------------------------------------------

-- Both views select the column, so they block the rewrite below.  They are
-- recreated at the end of this migration, without the triple.
DROP VIEW v_generator_params;
DROP VIEW v_observation;

ALTER TYPE e_execution_requirement RENAME TO e_scheduling_mode;

ALTER TABLE t_observation
  RENAME COLUMN c_execution_requirement TO c_scheduling_mode;

-- Backfill, carried by a no-op type change rather than an UPDATE.  Two things
-- have to happen at once: V1245 made the column nullable ("not specified"),
-- which the mode has no use for, and the floor about to be dropped has to be
-- baked into the stored value or every existing row loses it.
--
-- So this is execution_requirement_effective() evaluated one last time, with
-- 'interrupting' added on top.  A rapid ToO carrying no explicit requirement was
-- *effectively* uninterruptible, and must stay uninterruptible -- writing the
-- explicit value alone would quietly make it splittable and interruptible again,
-- and (once the activation is derived in the next migration) demote it from rapid
-- to standard.  GREATEST is what the dropped function used, and the ladder
-- ordering makes it mean the same thing here.
--
-- ALTER ... TYPE ... USING rewrites the table as DDL, so unlike an UPDATE it
-- queues no deferred trigger events and the ALTERs below can follow it in the
-- same transaction.  This is the trap that split V1238 from V1239 and put V1245's
-- backfill last.
ALTER TABLE t_observation
  ALTER COLUMN c_scheduling_mode TYPE e_scheduling_mode
    USING GREATEST(
      COALESCE(c_scheduling_mode, 'unconstrained'::e_scheduling_mode),
      CASE c_too_activation
        WHEN 'interrupting'::e_too_activation THEN 'interrupting'::e_scheduling_mode
        WHEN 'rapid'::e_too_activation        THEN 'uninterruptible'::e_scheduling_mode
        ELSE                                       'unconstrained'::e_scheduling_mode
      END
    );

ALTER TABLE t_observation
  ALTER COLUMN c_scheduling_mode SET NOT NULL,
  ALTER COLUMN c_scheduling_mode SET DEFAULT 'unconstrained'::e_scheduling_mode;

-- The floor and the default it was built from.  Nothing else references them.
DROP FUNCTION execution_requirement_effective(e_scheduling_mode, e_too_activation);
DROP FUNCTION execution_requirement_default(e_too_activation);

-------------------------------------------------------------------------------
-- Views.
-------------------------------------------------------------------------------

-- v_observation is `SELECT o.*`, so it picks up c_scheduling_mode on its own.
-- Body otherwise copied verbatim from V1245, less the two triple columns; the
-- deprecated c_is_splittable now reads straight off the mode, with no floor in
-- between.
CREATE VIEW v_observation AS
  SELECT o.*,
  (o.c_scheduling_mode = 'unconstrained'::e_scheduling_mode) AS c_is_splittable,
  CASE WHEN o.c_explicit_ra              IS NOT NULL THEN o.c_observation_id END AS c_explicit_base_id,
  CASE WHEN o.c_air_mass_min             IS NOT NULL THEN o.c_observation_id END AS c_air_mass_id,
  CASE WHEN o.c_hour_angle_min           IS NOT NULL THEN o.c_observation_id END AS c_hour_angle_id,
  CASE WHEN o.c_observing_mode_type      IS NOT NULL THEN o.c_observation_id END AS c_observing_mode_id,
  CASE WHEN o.c_spec_wavelength          IS NOT NULL THEN o.c_observation_id END AS c_spec_wavelength_id,
  CASE WHEN o.c_spec_wavelength_coverage IS NOT NULL THEN o.c_observation_id END AS c_spec_wavelength_coverage_id,
  CASE WHEN o.c_spec_focal_plane_angle   IS NOT NULL THEN o.c_observation_id END AS c_spec_focal_plane_angle_id,
  CASE WHEN o.c_img_minimum_fov          IS NOT NULL THEN o.c_observation_id END AS c_img_minimum_fov_id,
  CASE WHEN o.c_observation_duration     IS NOT NULL THEN o.c_observation_id END AS c_observation_duration_id,
  CASE WHEN o.c_orig_est_setup_count     IS NOT NULL THEN o.c_observation_id END AS c_original_estimate_id,
  CASE WHEN o.c_science_mode = 'imaging'::d_tag      THEN o.c_observation_id END AS c_imaging_mode_id,
  CASE WHEN o.c_science_mode = 'spectroscopy'::d_tag THEN o.c_observation_id END AS c_spectroscopy_mode_id,
  c.c_active_start::timestamp + (c.c_active_end::timestamp - c.c_active_start::timestamp) * 0.5 AS c_reference_time,
  EXISTS (
    SELECT 1
    FROM t_sequence_materialization m
    WHERE m.c_observation_id = o.c_observation_id
      AND m.c_sequence_type = 'science'::e_sequence_type
  ) AS c_science_sequence_is_materialized,
  EXISTS (
    SELECT 1
    FROM t_sequence_materialization m
    WHERE m.c_observation_id = o.c_observation_id
      AND m.c_sequence_type = 'acquisition'::e_sequence_type
  ) AS c_acquisition_sequence_is_materialized,
  (
    SELECT a.c_target_id
    FROM t_asterism_target a
    WHERE a.c_observation_id = o.c_observation_id
      AND a.c_is_signal_to_noise_target
  ) AS c_signal_to_noise_target_id
  FROM t_observation o
  LEFT JOIN t_proposal p on p.c_program_id = o.c_program_id
  LEFT JOIN t_cfp c on p.c_cfp_id = c.c_cfp_id;

-- v_generator_params as in V1245, except that the effective execution
-- requirement is now just the mode.  The generator consumes only .isSplittable
-- from it.
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
  o.c_declared_state,
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
  END AS c_execution_state,
  COALESCE(s_counts.c_step_count, 0) AS c_step_count,
  o.c_scheduling_mode,
  o.c_blind_offset_target_id,
  b.c_sid_rv AS c_blind_rv,
  b.c_source_profile AS c_blind_source_profile,
  t.c_target_id,
  t.c_sid_rv,
  t.c_source_profile,
  COALESCE(t.c_is_signal_to_noise_target, false) AS c_is_signal_to_noise_target
FROM
  t_observation o
LEFT JOIN t_target b ON b.c_target_id = o.c_blind_offset_target_id
LEFT JOIN LATERAL (
  SELECT t.c_target_id,
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

-------------------------------------------------------------------------------
-- Documentation.
-------------------------------------------------------------------------------

COMMENT ON TYPE e_scheduling_mode IS
  'What the Scheduler may do with an observation, as a ladder in which each '
  'value keeps every restriction below it and adds one.  Note that no_splitting '
  'may still be interrupted -- interrupting it abandons the visit rather than '
  'resuming it -- and that interrupting is itself uninterruptible, so no Target '
  'of Opportunity can ever preempt another.';

COMMENT ON COLUMN t_observation.c_scheduling_mode IS
  'The observation''s scheduling mode.  Together with whether its asterism holds '
  'an opportunity target this determines the ToO activation; interrupting '
  'additionally requires an opportunity target, which the workflow validates.';
