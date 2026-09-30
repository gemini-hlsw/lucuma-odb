-- Reacquisitions (recentering on the target between full setups) join the
-- execution digest beside the setup count.  Only spectroscopy guided by a PWFS
-- is expected to need them, so the generator now needs the effective guide
-- probe: the explicit probe, else the mode default, which depends on whether
-- the asterism is nonsidereal.

-- Obscalc: nullable like the other digest columns (null when there is no digest).
ALTER TABLE t_obscalc
  ADD COLUMN c_reacquisition_count int4 NULL CHECK (c_reacquisition_count >= 0);

-- Existing digests predate reacquisitions and did not charge for them.
UPDATE t_obscalc
   SET c_reacquisition_count = 0
 WHERE c_setup_count IS NOT NULL;

-- The values come only from a fresh calculation, so recompute every digest.
UPDATE t_obscalc SET
  c_last_invalidation = NOW(),
  c_failure_count     = 0,
  c_retry_at          = NULL,
  c_obscalc_state     = 'pending'
WHERE c_obscalc_state IN ('ready', 'retry');

TRUNCATE TABLE t_execution_digest;

ALTER TABLE t_execution_digest
  ADD COLUMN c_reacquisition_count int4 NOT NULL CHECK (c_reacquisition_count >= 0);

-- Original estimate: joins the all-or-none set.  Estimates recorded before this
-- column existed charged no reacquisitions, so they are backfilled with 0.
ALTER TABLE t_observation
  ADD COLUMN c_orig_est_reacquisition_count int4 NULL CHECK (c_orig_est_reacquisition_count >= 0);

UPDATE t_observation
   SET c_orig_est_reacquisition_count = 0
 WHERE c_orig_est_setup_count IS NOT NULL;

SET CONSTRAINTS ALL IMMEDIATE;

ALTER TABLE t_observation
  DROP CONSTRAINT original_estimate_all_or_none,
  ADD CONSTRAINT original_estimate_all_or_none CHECK (
    num_nonnulls(
      c_orig_est_full_setup_time,
      c_orig_est_reacq_setup_time,
      c_orig_est_setup_count,
      c_orig_est_reacquisition_count,
      c_orig_est_calibration_count,
      c_orig_est_sci_non_charged_time,
      c_orig_est_sci_program_time,
      c_orig_est_total_non_charged_time,
      c_orig_est_total_program_time
    ) IN (0, 9)
  );

-- v_observation selects o.*, so it must be recreated to pick up the new column.
DROP VIEW v_observation;

-- Body copied verbatim from V1328.
CREATE VIEW v_observation AS
  SELECT o.*,
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
  CASE WHEN o.c_altair_mode              IS NOT NULL THEN o.c_observation_id END AS c_altair_id,
  -- Alias read as a plain nullable column; c_altair_cass_rotator itself is mapped
  -- as a non-null field of the nested Altair object and a grackle ColumnRef is
  -- identified by name alone, so the two uses need distinct names.
  o.c_altair_cass_rotator AS c_cass_rotator,
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
  ) AS c_signal_to_noise_target_id,
  o.c_altair_mode AS c_configuration_altair_mode
  FROM t_observation o
  LEFT JOIN t_proposal p on p.c_program_id = o.c_program_id
  LEFT JOIN t_cfp c on p.c_cfp_id = c.c_cfp_id;

-- The generator needs the explicit guide probe and whether the asterism is
-- nonsidereal to work out the effective guide probe.  Columns are appended, so
-- the view may be replaced in place.  Body otherwise copied from V1328.
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
  COALESCE(t.c_is_signal_to_noise_target, false) AS c_is_signal_to_noise_target,
  o.c_altair_mode,
  o.c_altair_field_lens,
  o.c_altair_cass_rotator,
  o.c_altair_nd_filter,
  o.c_explicit_guide_probe,
  COALESCE(t.c_type = 'nonsidereal'::e_target_type, false) AS c_is_nonsidereal
FROM
  t_observation o
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
