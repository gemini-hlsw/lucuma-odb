
---
--- REMOVE 'for_review' FROM e_workflow_state
---

ALTER TYPE e_workflow_state RENAME TO e_workflow_state_old;
CREATE TYPE e_workflow_state
  AS ENUM('inactive', 'undefined', 'unapproved', 'defined', 'ready', 'ongoing', 'completed');

CREATE OR REPLACE FUNCTION convert_workflow_array(arr e_workflow_state_old[])
RETURNS e_workflow_state[] AS $$
  SELECT COALESCE(arr::text[]::e_workflow_state[], '{}'::e_workflow_state[]);
$$ LANGUAGE sql IMMUTABLE;

ALTER TABLE t_obscalc 
  -- c_workflow_state
  ALTER c_workflow_state DROP DEFAULT,
  ALTER c_workflow_state TYPE e_workflow_state 
    USING CASE 
      WHEN c_workflow_state::text = 'for_review' THEN 'defined'::e_workflow_state
      ELSE c_workflow_state::text::e_workflow_state
    END,
  ALTER c_workflow_state SET DEFAULT 'undefined',
  -- c_workflow_transitions
  ALTER c_workflow_transitions DROP DEFAULT,
  ALTER COLUMN c_workflow_transitions TYPE e_workflow_state[] 
  USING convert_workflow_array(array_remove(c_workflow_transitions, 'for_review'::e_workflow_state_old)),
  ALTER c_workflow_transitions SET DEFAULT ARRAY['inactive'::e_workflow_state];

DROP FUNCTION convert_workflow_array;
DROP TYPE e_workflow_state_old;

-- Fix itc_version_update(), from V1289
CREATE OR REPLACE FUNCTION itc_version_update()
  RETURNS trigger AS $$
BEGIN
  NEW.c_last_update = NOW();
  IF (OLD.c_version IS DISTINCT FROM NEW.c_version OR OLD.c_data IS DISTINCT FROM NEW.c_data) THEN
    DELETE FROM t_itc_result WHERE NOT c_is_frozen;

    -- Reset to pending, but only for observations in a committed workflow state
    -- 'ready' or non-executed ones with itc errors.
    UPDATE t_obscalc SET
      c_last_invalidation = NOW(),
      c_failure_count     = 0,
      c_retry_at          = NULL,
      c_obscalc_state     = 'pending'
    WHERE c_obscalc_state IN ('ready', 'retry')
      AND c_workflow_state NOT IN ('inactive', 'ongoing', 'completed')
      AND (
            c_workflow_state = 'ready'
            OR c_workflow_validations @> '[{"code": "ITC_ERROR"}]'::jsonb
          );
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

---
--- REMOVE 'for_review' FROM e_workflow_user_state
---

ALTER TYPE e_workflow_user_state RENAME TO e_workflow_user_state_old;
CREATE TYPE e_workflow_user_state
  AS ENUM('inactive', 'ready');

DROP VIEW v_observation;
DROP TRIGGER too_trigger_track_ready_trigger ON t_observation;
DROP TRIGGER prune_aeon_required_instruments ON t_observation;

ALTER TABLE t_observation
  ALTER c_workflow_user_state TYPE e_workflow_user_state
    USING CASE 
      WHEN c_workflow_user_state::text = 'for_review' THEN null
      ELSE c_workflow_user_state::text::e_workflow_user_state
    END;

-- Body copied verbatim from V1261; all we're doing is re-creating to change the type of c_workflow_user_state
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

CREATE TRIGGER too_trigger_track_ready_trigger
  AFTER INSERT OR UPDATE OF c_workflow_user_state, c_scheduling_mode, c_has_too_target, c_has_unresolved_too_target ON t_observation
  FOR EACH ROW
  EXECUTE FUNCTION too_trigger_track_ready();

-- From V1287
CREATE TRIGGER prune_aeon_required_instruments
AFTER DELETE OR UPDATE OF c_existence, c_workflow_user_state, c_instrument, c_calibration_role
ON t_observation
FOR EACH ROW
EXECUTE FUNCTION prune_aeon_required_instruments();

DROP TYPE e_workflow_user_state_old;
