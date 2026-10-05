-- Calibration estimate: the unobserved tellurics already in the group (each at
-- its own estimate) and those still predicted (each at the group's average),
-- as counts and times.  Together they are the calibration count, which is no
-- longer stored.  Only the expected time joins the observation's total, since
-- existing tellurics are observations of their own.  Computed by the generator,
-- so stored beside the other digest values in both digest tables and frozen in
-- the original estimate.

ALTER TABLE t_obscalc
  DROP COLUMN c_calibration_count,
  ADD COLUMN c_exist_cal_count            int4     NULL CHECK (c_exist_cal_count            >= 0),
  ADD COLUMN c_exist_cal_non_charged_time interval NULL CHECK (c_exist_cal_non_charged_time >= interval '0 seconds'),
  ADD COLUMN c_exist_cal_program_time     interval NULL CHECK (c_exist_cal_program_time     >= interval '0 seconds'),
  ADD COLUMN c_exp_cal_count              int4     NULL CHECK (c_exp_cal_count              >= 0),
  ADD COLUMN c_exp_cal_non_charged_time   interval NULL CHECK (c_exp_cal_non_charged_time   >= interval '0 seconds'),
  ADD COLUMN c_exp_cal_program_time       interval NULL CHECK (c_exp_cal_program_time       >= interval '0 seconds');

-- Existing digests read as zero until their next natural recalculation; a
-- partial null would not decode, so the value is filled rather than left.
UPDATE t_obscalc
   SET c_exist_cal_count            = 0,
       c_exist_cal_non_charged_time = interval '0 seconds',
       c_exist_cal_program_time     = interval '0 seconds',
       c_exp_cal_count              = 0,
       c_exp_cal_non_charged_time   = interval '0 seconds',
       c_exp_cal_program_time       = interval '0 seconds'
 WHERE c_setup_count IS NOT NULL;

-- Cached digests keep their rows and read as zero until regenerated.
ALTER TABLE t_execution_digest
  DROP COLUMN c_calibration_count,
  ADD COLUMN c_exist_cal_count            int4     NOT NULL DEFAULT 0 CHECK (c_exist_cal_count >= 0),
  ADD COLUMN c_exist_cal_non_charged_time interval NOT NULL DEFAULT interval '0 seconds'
    CHECK (c_exist_cal_non_charged_time >= interval '0 seconds'),
  ADD COLUMN c_exist_cal_program_time     interval NOT NULL DEFAULT interval '0 seconds'
    CHECK (c_exist_cal_program_time     >= interval '0 seconds'),
  ADD COLUMN c_exp_cal_count              int4     NOT NULL DEFAULT 0 CHECK (c_exp_cal_count >= 0),
  ADD COLUMN c_exp_cal_non_charged_time   interval NOT NULL DEFAULT interval '0 seconds'
    CHECK (c_exp_cal_non_charged_time   >= interval '0 seconds'),
  ADD COLUMN c_exp_cal_program_time       interval NOT NULL DEFAULT interval '0 seconds'
    CHECK (c_exp_cal_program_time       >= interval '0 seconds');

-- Original estimate: joins the all-or-none set, so recorded estimates take 0.
ALTER TABLE t_observation
  ADD COLUMN c_orig_est_exist_cal_count            int4     NULL CHECK (c_orig_est_exist_cal_count            >= 0),
  ADD COLUMN c_orig_est_exist_cal_non_charged_time interval NULL CHECK (c_orig_est_exist_cal_non_charged_time >= interval '0 seconds'),
  ADD COLUMN c_orig_est_exist_cal_program_time     interval NULL CHECK (c_orig_est_exist_cal_program_time     >= interval '0 seconds'),
  ADD COLUMN c_orig_est_exp_cal_count              int4     NULL CHECK (c_orig_est_exp_cal_count              >= 0),
  ADD COLUMN c_orig_est_exp_cal_non_charged_time   interval NULL CHECK (c_orig_est_exp_cal_non_charged_time   >= interval '0 seconds'),
  ADD COLUMN c_orig_est_exp_cal_program_time       interval NULL CHECK (c_orig_est_exp_cal_program_time       >= interval '0 seconds');

UPDATE t_observation
   SET c_orig_est_exist_cal_count            = 0,
       c_orig_est_exist_cal_non_charged_time = interval '0 seconds',
       c_orig_est_exist_cal_program_time     = interval '0 seconds',
       c_orig_est_exp_cal_count              = 0,
       c_orig_est_exp_cal_non_charged_time   = interval '0 seconds',
       c_orig_est_exp_cal_program_time       = interval '0 seconds'
 WHERE c_orig_est_setup_count IS NOT NULL;

SET CONSTRAINTS ALL IMMEDIATE;

ALTER TABLE t_observation
  DROP CONSTRAINT original_estimate_all_or_none,
  ADD CONSTRAINT original_estimate_all_or_none CHECK (
    num_nonnulls(
      c_orig_est_full_setup_time,
      c_orig_est_reacq_setup_time,
      c_orig_est_setup_count,
      c_orig_est_exist_cal_count,
      c_orig_est_exist_cal_non_charged_time,
      c_orig_est_exist_cal_program_time,
      c_orig_est_exp_cal_count,
      c_orig_est_exp_cal_non_charged_time,
      c_orig_est_exp_cal_program_time,
      c_orig_est_sci_non_charged_time,
      c_orig_est_sci_program_time,
      c_orig_est_total_non_charged_time,
      c_orig_est_total_program_time
    ) IN (0, 13)
  );

-- v_observation selects o.*, so it must be recreated to pick up the new columns.
DROP VIEW v_observation;

ALTER TABLE t_observation
  DROP COLUMN c_orig_est_calibration_count;

-- Body copied verbatim from V1335.
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
  o.c_altair_mode AS c_configuration_altair_mode,
  EXISTS (
    SELECT 1
    FROM t_sequence_materialization m
    WHERE m.c_observation_id = o.c_observation_id
      AND m.c_sequence_type = 'science'::e_sequence_type
      AND m.c_customized
  ) AS c_science_sequence_is_customized,
  EXISTS (
    SELECT 1
    FROM t_sequence_materialization m
    WHERE m.c_observation_id = o.c_observation_id
      AND m.c_sequence_type = 'acquisition'::e_sequence_type
      AND m.c_customized
  ) AS c_acquisition_sequence_is_customized
  FROM t_observation o
  LEFT JOIN t_proposal p on p.c_program_id = o.c_program_id
  LEFT JOIN t_cfp c on p.c_cfp_id = c.c_cfp_id;

-- The charge depends on the tellurics in the science observation's group and
-- their totals, so a telluric appearing, disappearing, being declined or
-- reinstated, getting its first visit, or settling on a different total
-- invalidates the science observation's digest.
CREATE OR REPLACE FUNCTION invalidate_science_obscalc_for_telluric(
  telluric_group_id d_group_id
) RETURNS void AS $$
DECLARE
  science_id d_observation_id;
BEGIN
  IF telluric_group_id IS NULL THEN
    RETURN;
  END IF;
  SELECT c_observation_id INTO science_id
  FROM   t_observation
  WHERE  c_group_id         = telluric_group_id
    AND  c_calibration_role IS NULL
    AND  c_existence        = 'present'
  LIMIT 1;
  IF FOUND THEN
    CALL invalidate_obscalc(science_id);
  END IF;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION telluric_change_obscalc_invalidate()
RETURNS TRIGGER AS $$
BEGIN
  IF TG_OP = 'DELETE' THEN
    IF OLD.c_calibration_role = 'telluric' THEN
      PERFORM invalidate_science_obscalc_for_telluric(OLD.c_group_id);
    END IF;
    RETURN OLD;
  END IF;
  IF NEW.c_calibration_role = 'telluric' AND (
       TG_OP = 'INSERT'
    OR NEW.c_existence           IS DISTINCT FROM OLD.c_existence
    OR NEW.c_workflow_user_state IS DISTINCT FROM OLD.c_workflow_user_state
    OR NEW.c_group_id            IS DISTINCT FROM OLD.c_group_id
  ) THEN
    PERFORM invalidate_science_obscalc_for_telluric(NEW.c_group_id);
    IF TG_OP = 'UPDATE' AND NEW.c_group_id IS DISTINCT FROM OLD.c_group_id THEN
      PERFORM invalidate_science_obscalc_for_telluric(OLD.c_group_id);
    END IF;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER telluric_change_obscalc_invalidate_trigger
  AFTER INSERT OR DELETE OR UPDATE OF c_existence, c_workflow_user_state, c_group_id ON t_observation
  FOR EACH ROW
  EXECUTE FUNCTION telluric_change_obscalc_invalidate();

CREATE OR REPLACE FUNCTION telluric_visit_obscalc_invalidate()
RETURNS TRIGGER AS $$
DECLARE
  telluric_group_id d_group_id;
BEGIN
  SELECT c_group_id INTO telluric_group_id
  FROM   t_observation
  WHERE  c_observation_id  = NEW.c_observation_id
    AND  c_calibration_role = 'telluric';
  IF FOUND THEN
    PERFORM invalidate_science_obscalc_for_telluric(telluric_group_id);
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER telluric_visit_obscalc_invalidate_trigger
  AFTER INSERT ON t_visit
  FOR EACH ROW
  EXECUTE FUNCTION telluric_visit_obscalc_invalidate();

-- Only a changed total re-runs the science, which stops the science -> telluric
-- resolution -> telluric digest -> science cycle once the numbers settle.  The
-- state is not checked: a result computed while the row was re-invalidated is
-- stored as 'pending' with its digest, and the recompute that follows usually
-- stores the same totals again, so waiting for 'ready' would miss the change.
CREATE OR REPLACE FUNCTION telluric_total_obscalc_invalidate()
RETURNS TRIGGER AS $$
DECLARE
  telluric_group_id d_group_id;
BEGIN
  IF NEW.c_last_update IS DISTINCT FROM OLD.c_last_update
     AND (   NEW.c_full_setup_time          IS DISTINCT FROM OLD.c_full_setup_time
          OR NEW.c_setup_count              IS DISTINCT FROM OLD.c_setup_count
          OR NEW.c_reacq_setup_time         IS DISTINCT FROM OLD.c_reacq_setup_time
          OR NEW.c_reacquisition_count      IS DISTINCT FROM OLD.c_reacquisition_count
          OR NEW.c_sci_obs_class            IS DISTINCT FROM OLD.c_sci_obs_class
          OR NEW.c_sci_non_charged_time     IS DISTINCT FROM OLD.c_sci_non_charged_time
          OR NEW.c_sci_program_time         IS DISTINCT FROM OLD.c_sci_program_time
          OR NEW.c_exp_cal_non_charged_time IS DISTINCT FROM OLD.c_exp_cal_non_charged_time
          OR NEW.c_exp_cal_program_time     IS DISTINCT FROM OLD.c_exp_cal_program_time) THEN
    SELECT c_group_id INTO telluric_group_id
    FROM   t_observation
    WHERE  c_observation_id  = NEW.c_observation_id
      AND  c_calibration_role = 'telluric';
    IF FOUND THEN
      PERFORM invalidate_science_obscalc_for_telluric(telluric_group_id);
    END IF;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER telluric_total_obscalc_invalidate_trigger
  AFTER UPDATE OF c_last_update ON t_obscalc
  FOR EACH ROW
  EXECUTE FUNCTION telluric_total_obscalc_invalidate();

-- Existing digests carry zero calibrations, so recompute the science
-- observations of every mode that takes tellurics.
DO $$
DECLARE
  obs_id d_observation_id;
BEGIN
  FOR obs_id IN
    SELECT c_observation_id
    FROM   t_observation
    WHERE  c_existence        = 'present'
      AND  c_calibration_role IS NULL
      AND  c_observing_mode_type IN (
             'flamingos_2_long_slit',
             'flamingos_2_mos',
             'igrins_2_long_slit',
             'gnirs_long_slit',
             'gnirs_ifu'
           )
  LOOP
    CALL invalidate_obscalc(obs_id);
  END LOOP;
END;
$$;
