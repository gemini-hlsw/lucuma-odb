-- Two additions to the execution digest, both computed by the generator:
--
-- * c_calibration_count: expected number of calibration epochs for an
--   observation, carried beside the setup count; calibration observations
--   report 0.
-- * Per sequence (acq, sci): bias, dark, arc, flat and observing (all other)
--   step counts and times, so the five sum to the sequence time estimate per
--   charge class.

-- Obscalc: nullable like the other digest columns (null when there is no digest).
ALTER TABLE t_obscalc
  ADD COLUMN c_calibration_count              int4     NULL CHECK (c_calibration_count              >= 0),
  ADD COLUMN c_acq_bias_count                 int4     NULL CHECK (c_acq_bias_count                 >= 0),
  ADD COLUMN c_acq_bias_non_charged_time      interval NULL CHECK (c_acq_bias_non_charged_time      >= interval '0 seconds'),
  ADD COLUMN c_acq_bias_program_time          interval NULL CHECK (c_acq_bias_program_time          >= interval '0 seconds'),
  ADD COLUMN c_acq_dark_count                 int4     NULL CHECK (c_acq_dark_count                 >= 0),
  ADD COLUMN c_acq_dark_non_charged_time      interval NULL CHECK (c_acq_dark_non_charged_time      >= interval '0 seconds'),
  ADD COLUMN c_acq_dark_program_time          interval NULL CHECK (c_acq_dark_program_time          >= interval '0 seconds'),
  ADD COLUMN c_acq_arc_count                  int4     NULL CHECK (c_acq_arc_count                  >= 0),
  ADD COLUMN c_acq_arc_non_charged_time       interval NULL CHECK (c_acq_arc_non_charged_time       >= interval '0 seconds'),
  ADD COLUMN c_acq_arc_program_time           interval NULL CHECK (c_acq_arc_program_time           >= interval '0 seconds'),
  ADD COLUMN c_acq_flat_count                 int4     NULL CHECK (c_acq_flat_count                 >= 0),
  ADD COLUMN c_acq_flat_non_charged_time      interval NULL CHECK (c_acq_flat_non_charged_time      >= interval '0 seconds'),
  ADD COLUMN c_acq_flat_program_time          interval NULL CHECK (c_acq_flat_program_time          >= interval '0 seconds'),
  ADD COLUMN c_acq_observing_count            int4     NULL CHECK (c_acq_observing_count            >= 0),
  ADD COLUMN c_acq_observing_non_charged_time interval NULL CHECK (c_acq_observing_non_charged_time >= interval '0 seconds'),
  ADD COLUMN c_acq_observing_program_time     interval NULL CHECK (c_acq_observing_program_time     >= interval '0 seconds'),
  ADD COLUMN c_sci_bias_count                 int4     NULL CHECK (c_sci_bias_count                 >= 0),
  ADD COLUMN c_sci_bias_non_charged_time      interval NULL CHECK (c_sci_bias_non_charged_time      >= interval '0 seconds'),
  ADD COLUMN c_sci_bias_program_time          interval NULL CHECK (c_sci_bias_program_time          >= interval '0 seconds'),
  ADD COLUMN c_sci_dark_count                 int4     NULL CHECK (c_sci_dark_count                 >= 0),
  ADD COLUMN c_sci_dark_non_charged_time      interval NULL CHECK (c_sci_dark_non_charged_time      >= interval '0 seconds'),
  ADD COLUMN c_sci_dark_program_time          interval NULL CHECK (c_sci_dark_program_time          >= interval '0 seconds'),
  ADD COLUMN c_sci_arc_count                  int4     NULL CHECK (c_sci_arc_count                  >= 0),
  ADD COLUMN c_sci_arc_non_charged_time       interval NULL CHECK (c_sci_arc_non_charged_time       >= interval '0 seconds'),
  ADD COLUMN c_sci_arc_program_time           interval NULL CHECK (c_sci_arc_program_time           >= interval '0 seconds'),
  ADD COLUMN c_sci_flat_count                 int4     NULL CHECK (c_sci_flat_count                 >= 0),
  ADD COLUMN c_sci_flat_non_charged_time      interval NULL CHECK (c_sci_flat_non_charged_time      >= interval '0 seconds'),
  ADD COLUMN c_sci_flat_program_time          interval NULL CHECK (c_sci_flat_program_time          >= interval '0 seconds'),
  ADD COLUMN c_sci_observing_count            int4     NULL CHECK (c_sci_observing_count            >= 0),
  ADD COLUMN c_sci_observing_non_charged_time interval NULL CHECK (c_sci_observing_non_charged_time >= interval '0 seconds'),
  ADD COLUMN c_sci_observing_program_time     interval NULL CHECK (c_sci_observing_program_time     >= interval '0 seconds');

-- Existing digests predate both additions: no epochs were counted and there
-- is no step breakdown, so all of their time counts as observing time (keeping
-- the sum invariant) until they are recomputed.
UPDATE t_obscalc
   SET c_calibration_count              = 0,
       c_acq_bias_count                 = 0,
       c_acq_bias_non_charged_time      = interval '0 seconds',
       c_acq_bias_program_time          = interval '0 seconds',
       c_acq_dark_count                 = 0,
       c_acq_dark_non_charged_time      = interval '0 seconds',
       c_acq_dark_program_time          = interval '0 seconds',
       c_acq_arc_count                  = 0,
       c_acq_arc_non_charged_time       = interval '0 seconds',
       c_acq_arc_program_time           = interval '0 seconds',
       c_acq_flat_count                 = 0,
       c_acq_flat_non_charged_time      = interval '0 seconds',
       c_acq_flat_program_time          = interval '0 seconds',
       c_acq_observing_count            = 0,
       c_acq_observing_non_charged_time = c_acq_non_charged_time,
       c_acq_observing_program_time     = c_acq_program_time,
       c_sci_bias_count                 = 0,
       c_sci_bias_non_charged_time      = interval '0 seconds',
       c_sci_bias_program_time          = interval '0 seconds',
       c_sci_dark_count                 = 0,
       c_sci_dark_non_charged_time      = interval '0 seconds',
       c_sci_dark_program_time          = interval '0 seconds',
       c_sci_arc_count                  = 0,
       c_sci_arc_non_charged_time       = interval '0 seconds',
       c_sci_arc_program_time           = interval '0 seconds',
       c_sci_flat_count                 = 0,
       c_sci_flat_non_charged_time      = interval '0 seconds',
       c_sci_flat_program_time          = interval '0 seconds',
       c_sci_observing_count            = 0,
       c_sci_observing_non_charged_time = c_sci_non_charged_time,
       c_sci_observing_program_time     = c_sci_program_time
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
  ADD COLUMN c_calibration_count              int4     NOT NULL CHECK (c_calibration_count              >= 0),
  ADD COLUMN c_acq_bias_count                 int4     NOT NULL CHECK (c_acq_bias_count                 >= 0),
  ADD COLUMN c_acq_bias_non_charged_time      interval NOT NULL CHECK (c_acq_bias_non_charged_time      >= interval '0 seconds'),
  ADD COLUMN c_acq_bias_program_time          interval NOT NULL CHECK (c_acq_bias_program_time          >= interval '0 seconds'),
  ADD COLUMN c_acq_dark_count                 int4     NOT NULL CHECK (c_acq_dark_count                 >= 0),
  ADD COLUMN c_acq_dark_non_charged_time      interval NOT NULL CHECK (c_acq_dark_non_charged_time      >= interval '0 seconds'),
  ADD COLUMN c_acq_dark_program_time          interval NOT NULL CHECK (c_acq_dark_program_time          >= interval '0 seconds'),
  ADD COLUMN c_acq_arc_count                  int4     NOT NULL CHECK (c_acq_arc_count                  >= 0),
  ADD COLUMN c_acq_arc_non_charged_time       interval NOT NULL CHECK (c_acq_arc_non_charged_time       >= interval '0 seconds'),
  ADD COLUMN c_acq_arc_program_time           interval NOT NULL CHECK (c_acq_arc_program_time           >= interval '0 seconds'),
  ADD COLUMN c_acq_flat_count                 int4     NOT NULL CHECK (c_acq_flat_count                 >= 0),
  ADD COLUMN c_acq_flat_non_charged_time      interval NOT NULL CHECK (c_acq_flat_non_charged_time      >= interval '0 seconds'),
  ADD COLUMN c_acq_flat_program_time          interval NOT NULL CHECK (c_acq_flat_program_time          >= interval '0 seconds'),
  ADD COLUMN c_acq_observing_count            int4     NOT NULL CHECK (c_acq_observing_count            >= 0),
  ADD COLUMN c_acq_observing_non_charged_time interval NOT NULL CHECK (c_acq_observing_non_charged_time >= interval '0 seconds'),
  ADD COLUMN c_acq_observing_program_time     interval NOT NULL CHECK (c_acq_observing_program_time     >= interval '0 seconds'),
  ADD COLUMN c_sci_bias_count                 int4     NOT NULL CHECK (c_sci_bias_count                 >= 0),
  ADD COLUMN c_sci_bias_non_charged_time      interval NOT NULL CHECK (c_sci_bias_non_charged_time      >= interval '0 seconds'),
  ADD COLUMN c_sci_bias_program_time          interval NOT NULL CHECK (c_sci_bias_program_time          >= interval '0 seconds'),
  ADD COLUMN c_sci_dark_count                 int4     NOT NULL CHECK (c_sci_dark_count                 >= 0),
  ADD COLUMN c_sci_dark_non_charged_time      interval NOT NULL CHECK (c_sci_dark_non_charged_time      >= interval '0 seconds'),
  ADD COLUMN c_sci_dark_program_time          interval NOT NULL CHECK (c_sci_dark_program_time          >= interval '0 seconds'),
  ADD COLUMN c_sci_arc_count                  int4     NOT NULL CHECK (c_sci_arc_count                  >= 0),
  ADD COLUMN c_sci_arc_non_charged_time       interval NOT NULL CHECK (c_sci_arc_non_charged_time       >= interval '0 seconds'),
  ADD COLUMN c_sci_arc_program_time           interval NOT NULL CHECK (c_sci_arc_program_time           >= interval '0 seconds'),
  ADD COLUMN c_sci_flat_count                 int4     NOT NULL CHECK (c_sci_flat_count                 >= 0),
  ADD COLUMN c_sci_flat_non_charged_time      interval NOT NULL CHECK (c_sci_flat_non_charged_time      >= interval '0 seconds'),
  ADD COLUMN c_sci_flat_program_time          interval NOT NULL CHECK (c_sci_flat_program_time          >= interval '0 seconds'),
  ADD COLUMN c_sci_observing_count            int4     NOT NULL CHECK (c_sci_observing_count            >= 0),
  ADD COLUMN c_sci_observing_non_charged_time interval NOT NULL CHECK (c_sci_observing_non_charged_time >= interval '0 seconds'),
  ADD COLUMN c_sci_observing_program_time     interval NOT NULL CHECK (c_sci_observing_program_time     >= interval '0 seconds');

-- Original estimate: joins the all-or-none set.  Estimates recorded before this
-- column existed are backfilled with 0.
ALTER TABLE t_observation
  ADD COLUMN c_orig_est_calibration_count int4 NULL CHECK (c_orig_est_calibration_count >= 0);

UPDATE t_observation
   SET c_orig_est_calibration_count = 0
 WHERE c_orig_est_setup_count IS NOT NULL;

SET CONSTRAINTS ALL IMMEDIATE;

ALTER TABLE t_observation
  DROP CONSTRAINT original_estimate_all_or_none,
  ADD CONSTRAINT original_estimate_all_or_none CHECK (
    num_nonnulls(
      c_orig_est_full_setup_time,
      c_orig_est_reacq_setup_time,
      c_orig_est_setup_count,
      c_orig_est_calibration_count,
      c_orig_est_sci_non_charged_time,
      c_orig_est_sci_program_time,
      c_orig_est_total_non_charged_time,
      c_orig_est_total_program_time
    ) IN (0, 8)
  );

-- v_observation selects o.*, so it must be recreated to pick up the new column.
DROP VIEW v_observation;

-- Body copied verbatim from V1323.
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
