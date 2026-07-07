-- Records the execution time estimate as it stands when the first 'observe'
-- visit is recorded for an observation (the moment the execution sequence is
-- materialized and execution begins).  The obscalc estimate continues to
-- change (and reduce) as the observation executes; these columns preserve the
-- estimate at the start of execution so that it may be compared with the
-- actual time required after the fact.
--
-- The columns are null until the first observe visit is recorded, at which
-- point they are all set together in the same transaction that creates (or
-- claims) the visit.  They are never subsequently updated.
--
--   * full setup time, reacquisition setup time, and setup count correspond
--     to the SetupTime and setup count of the execution digest;
--   * sci(ence) non-charged / program times are the CategorizedTime estimate
--     for the science sequence;
--   * full non-charged / program times are the full time estimate: science
--     time plus full setup time for every setup, charged according to the
--     science sequence observe class.

ALTER TABLE t_observation
  ADD COLUMN c_orig_est_full_setup_time       interval NULL CHECK (c_orig_est_full_setup_time       >= interval '0 seconds'),
  ADD COLUMN c_orig_est_reacq_setup_time      interval NULL CHECK (c_orig_est_reacq_setup_time      >= interval '0 seconds'),
  ADD COLUMN c_orig_est_setup_count           int4     NULL CHECK (c_orig_est_setup_count           >= 0),
  ADD COLUMN c_orig_est_sci_non_charged_time  interval NULL CHECK (c_orig_est_sci_non_charged_time  >= interval '0 seconds'),
  ADD COLUMN c_orig_est_sci_program_time      interval NULL CHECK (c_orig_est_sci_program_time      >= interval '0 seconds'),
  ADD COLUMN c_orig_est_full_non_charged_time interval NULL CHECK (c_orig_est_full_non_charged_time >= interval '0 seconds'),
  ADD COLUMN c_orig_est_full_program_time     interval NULL CHECK (c_orig_est_full_program_time     >= interval '0 seconds'),

  -- The original estimate is recorded in a single step: either every column
  -- is null (not yet executed) or every column is set.
  ADD CONSTRAINT original_estimate_all_or_none CHECK (
    num_nonnulls(
      c_orig_est_full_setup_time,
      c_orig_est_reacq_setup_time,
      c_orig_est_setup_count,
      c_orig_est_sci_non_charged_time,
      c_orig_est_sci_program_time,
      c_orig_est_full_non_charged_time,
      c_orig_est_full_program_time
    ) IN (0, 7)
  );

-- Re-create v_observation from V1200, adding a synthetic id column that is
-- non-null only when the original estimate has been recorded.  (This is used
-- to determine the nullability of the corresponding GraphQL object.)
DROP VIEW v_observation;
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
  -- Scalar subquery: at most one flagged target per observation is guaranteed
  -- by the partial unique index i_asterism_single_sn_target. No LIMIT here on
  -- purpose -- if that invariant were ever violated, PostgreSQL raises "more
  -- than one row returned by a subquery used as an expression", surfacing the
  -- bug rather than hiding it.
  (
    SELECT a.c_target_id
    FROM t_asterism_target a
    WHERE a.c_observation_id = o.c_observation_id
      AND a.c_is_signal_to_noise_target
  ) AS c_signal_to_noise_target_id
  FROM t_observation o
  LEFT JOIN t_proposal p on p.c_program_id = o.c_program_id
  LEFT JOIN t_cfp c on p.c_cfp_id = c.c_cfp_id;
