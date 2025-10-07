-- Remove the CREATE OR REPLACE FUNCTION blindblind offsets from the t_asterism_target table and into the t_observation table

--------------------------------------------------------
-- Drop unnecessary triggers and functions
--------------------------------------------------------
-- from V1038
DROP FUNCTION IF EXISTS check_blind_offset_unique() CASCADE;

-- from V1040
DROP FUNCTION IF EXISTS check_blind_offset_not_shared() CASCADE;
DROP FUNCTION IF EXISTS delete_blind_offset_when_removed_from_asterism() CASCADE;
DROP FUNCTION IF EXISTS delete_move_blind_offset_when_use_flag_cleared() CASCADE;

--------------------------------------------------------
-- Add columns to t_observation for blind offset target and populate from t_asterism_target.
--------------------------------------------------------
ALTER TABLE t_observation
  ADD COLUMN c_explicit_blind_offset BOOLEAN NOT NULL DEFAULT FALSE,
  ADD COLUMN c_blind_offset_target_id d_target_id NULL,
  ADD FOREIGN KEY (c_blind_offset_target_id)
    REFERENCES t_target(c_target_id)
    ON DELETE SET NULL;

UPDATE t_observation o
SET c_blind_offset_target_id = t.c_target_id
FROM t_asterism_target a, t_target t
WHERE o.c_observation_id = a.c_observation_id
  AND a.c_target_id = t.c_target_id
  AND t.c_target_disposition = 'blind_offset';

--------------------------------------------------------
-- Remove blind offset targets from t_asterism_target
--------------------------------------------------------
DELETE FROM t_asterism_target a
USING t_target t
WHERE a.c_target_id = t.c_target_id
  AND t.c_target_disposition = 'blind_offset';

--------------------------------------------------------
-- Update v_observation to not look for the blind offset target in the asterism
-- Modied from V1038__blind_offset_asterism.sql
--------------------------------------------------------
DROP VIEW IF EXISTS v_observation;

CREATE OR REPLACE VIEW v_observation AS
  SELECT o.*,
  CASE WHEN o.c_explicit_ra              IS NOT NULL THEN o.c_observation_id END AS c_explicit_base_id,
  CASE WHEN o.c_air_mass_min             IS NOT NULL THEN o.c_observation_id END AS c_air_mass_id,
  CASE WHEN o.c_hour_angle_min           IS NOT NULL THEN o.c_observation_id END AS c_hour_angle_id,
  CASE WHEN o.c_observing_mode_type      IS NOT NULL THEN o.c_observation_id END AS c_observing_mode_id,
  CASE WHEN o.c_spec_wavelength          IS NOT NULL THEN o.c_observation_id END AS c_spec_wavelength_id,

  CASE WHEN o.c_exp_time_mode            IS NOT NULL THEN o.c_observation_id END AS c_exp_time_mode_id,
  CASE WHEN o.c_exp_time_mode = 'signal_to_noise'    THEN o.c_observation_id END AS c_etm_signal_to_noise_id,
  CASE WHEN o.c_exp_time_mode = 'time_and_count'     THEN o.c_observation_id END AS c_etm_time_and_count_id,
  CASE WHEN o.c_spec_wavelength_coverage IS NOT NULL THEN o.c_observation_id END AS c_spec_wavelength_coverage_id,
  CASE WHEN o.c_spec_focal_plane_angle   IS NOT NULL THEN o.c_observation_id END AS c_spec_focal_plane_angle_id,
  CASE WHEN o.c_img_minimum_fov          IS NOT NULL THEN o.c_observation_id END AS c_img_minimum_fov_id,
  CASE WHEN o.c_observation_duration     IS NOT NULL THEN o.c_observation_id END AS c_observation_duration_id,
  CASE WHEN o.c_science_mode = 'imaging'::d_tag      THEN o.c_observation_id END AS c_imaging_mode_id,
  CASE WHEN o.c_science_mode = 'spectroscopy'::d_tag THEN o.c_observation_id END AS c_spectroscopy_mode_id,
  c.c_active_start::timestamp + (c.c_active_end::timestamp - c.c_active_start::timestamp) * 0.5 AS c_reference_time
  FROM t_observation o
  LEFT JOIN t_proposal p on p.c_program_id = o.c_program_id
  LEFT JOIN t_cfp c on p.c_cfp_id = c.c_cfp_id
  LEFT JOIN t_gmos_north_long_slit mode_gnls ON o.c_observation_id = mode_gnls.c_observation_id
  LEFT JOIN t_gmos_south_long_slit mode_gsls ON o.c_observation_id = mode_gsls.c_observation_id
  LEFT JOIN t_gmos_north_imaging mode_gni ON o.c_observation_id = mode_gni.c_observation_id
  LEFT JOIN t_gmos_south_imaging mode_gsi ON o.c_observation_id = mode_gsi.c_observation_id;

--------------------------------------------------------
-- Update v_observation to not look for the blind offset target in the asterism
-- Modied from V1038
--------------------------------------------------------
DROP VIEW IF EXISTS v_generator_params;

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
  o.c_exp_time_mode,
  o.c_etm_signal_to_noise,
  o.c_etm_signal_to_noise_at,
  o.c_etm_exp_time,
  o.c_etm_exp_count,
  o.c_observing_mode_type,
  o.c_science_band,
  o.c_declared_complete,
  o.c_acq_reset_time,
  o.c_blind_offset_target_id,
  b.c_sid_rv AS c_blind_rv,
  b.c_source_profile AS c_blind_source_profile,
  t.c_target_id,
  t.c_sid_rv,
  t.c_source_profile
FROM
  t_observation o
LEFT JOIN t_target b ON c_target_id = o.c_blind_offset_target_id
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
ORDER BY
  o.c_observation_id,
  t.c_target_id;

--------------------------------------------------------
-- Update asterism_update - only science targets can be added to the asterism.
-- Modified from V1038 to what it was in V0916, but also only allow science and calibration targets in
-- the asterism
--------------------------------------------------------
CREATE OR REPLACE FUNCTION asterism_update()
  RETURNS trigger AS $$
DECLARE
  obsid d_observation_id;
BEGIN
  IF TG_OP = 'DELETE' THEN
    obsid := OLD.c_observation_id;
  ELSE
    IF EXISTS (
      SELECT 1 FROM t_target t
      WHERE t.c_target_id = NEW.c_target_id
        AND t.c_target_disposition NOT IN ('science', 'calibration')
    ) THEN
      RAISE EXCEPTION 'An asterism can only contain science and calibration targets';
    END IF;

    obsid := NEW.c_observation_id;
  END IF;

  update t_observation a 
  set c_asterism_group = coalesce(
    -- ensure that the lists are sorted so we can compare them
    (select to_json(array_agg(b.c_target_id order by b.c_target_id))::jsonb
    from t_asterism_target b
    where a.c_observation_id = b.c_observation_id),
    '[]'::jsonb
  ), c_title = (
    select array_to_string(
      coalesce(
          array_agg(coalesce(t.c_name, 'Unnamed') order by t.c_target_id), 
          array['Untargeted']
      ), 
      ', '
    )
    from t_asterism_target b
    join t_target t on b.c_target_id = t.c_target_id
    where a.c_observation_id = b.c_observation_id
      and t.c_existence = 'present'
  )
  where a.c_observation_id = obsid;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

--------------------------------------------------------
-- Update target_update - only science targets can be added to the asterism so checks can be removed
-- Modified from V1040 to what it was in V0868
--------------------------------------------------------
CREATE OR REPLACE FUNCTION target_update()
  RETURNS trigger AS $$
DECLARE
  tid d_target_id;
  oid d_observation_id;
BEGIN
  tid := NEW.c_target_id;

  -- update the titles of observations that use this target
  if (OLD.c_name != NEW.c_name OR OLD.c_existence != NEW.c_existence) THEN
    -- If one of the above changes, it indirectly triggers ch_observation_edit by modifying the obs title
    update t_observation a 
    set c_title = (
      select array_to_string(
        coalesce(
            array_agg(coalesce(t.c_name, 'Unnamed') order by t.c_target_id), 
            array['Untargeted']
        ), 
        ', '
      )
      from t_asterism_target b
      join t_target t on b.c_target_id = t.c_target_id
      where a.c_observation_id = b.c_observation_id
      and t.c_existence = 'present'
    )
    where a.c_observation_id in (
      select c_observation_id
      from t_asterism_target
      where c_target_id = tid
    );
  ELSE
    -- We'll directly trigger ch_observation_edit
    FOR oid in 
      select c_observation_id
      from t_asterism_target
      where c_target_id = tid
    LOOP
      PERFORM pg_notify('ch_observation_edit', oid || ',' || NEW.c_program_id  || ',' || 'UPDATE');
    END LOOP;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

--------------------------------------------------------
-- Update ensure_blind_offset_is_assigned to look in t_observation.
-- Check deferred to end of transaction
-- Modified from V1040
--------------------------------------------------------
CREATE OR REPLACE FUNCTION ensure_blind_offset_is_assigned()
RETURNS TRIGGER AS $$
BEGIN
  IF NEW.c_target_disposition = 'blind_offset' THEN
    IF NOT EXISTS (
      SELECT 1 FROM t_observation o
      WHERE o.c_blind_offset_target_id = NEW.c_target_id
    ) THEN
      RAISE EXCEPTION 'Blind offset targets must be assigned to an observation';
    END IF;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

--------------------------------------------------------
-- Replace the function with a typo it's name, delete_move_blind_offset_when_use_flag_cleared,
-- from V1040 - modify it to to directly delete the blind offset target when the flag is cleared.
--------------------------------------------------------
CREATE OR REPLACE FUNCTION delete_blind_offset_when_use_flag_cleared()
RETURNS TRIGGER AS $$
BEGIN
  IF (NEW.c_use_blind_offset = false AND OLD.c_use_blind_offset = true) THEN
    DELETE FROM t_target
    WHERE c_target_id = NEW.c_blind_offset_target_id;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_delete_blind_offset_when_use_flag_cleared
  AFTER UPDATE OF c_use_blind_offset ON t_observation
  FOR EACH ROW
  EXECUTE FUNCTION delete_blind_offset_when_use_flag_cleared();

--------------------------------------------------------
-- When the c_blind_offset_target_id column changes...
-- 1. Delete any old blind offset target.
-- 2. Ensure blind offsets aren't assigned to more than one observation.
-- 3. Make sure any assigned target has a blind_offset disposition.
--------------------------------------------------------
CREATE OR REPLACE FUNCTION on_blind_offset_target_id_change()
  RETURNS trigger AS $$
BEGIN
  IF (OLD.c_blind_offset_target_id IS NOT NULL) THEN
    DELETE FROM t_target 
    WHERE c_target_id = OLD.c_blind_offset_target_id;
  END IF;

  IF (NEW.c_blind_offset_target_id IS NOT NULL) THEN
    IF EXISTS(
      SELECT 1 FROM t_observation o
      WHERE o.c_observation_id != NEW.c_observation_id
          AND o.c_blind_offset_target_id IS NOT DISTINCT FROM NEW.c_blind_offset_target_id
    ) THEN
      RAISE EXCEPTION 'Blind offset targets cannot be shared between observations';
    ELSEIF EXISTS(
      SELECT 1 FROM t_target
      WHERE c_target_id = NEW.c_blind_offset_target_id
        AND c_target_disposition != 'blind_offset'
    ) THEN
      RAISE EXCEPTION 'Blind offset targets must have a disposition of blind_offset';
    END IF;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER trg_on_blind_offset_target_id_change
  AFTER INSERT OR UPDATE OF c_blind_offset_target_id on t_observation
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW
  EXECUTE FUNCTION on_blind_offset_target_id_change();

--------------------------------------------------------
-- Update blind_offset_existence_on_observation_existence to get blind offset target from t_observation.
-- Modified from V1040
--------------------------------------------------------
CREATE OR REPLACE FUNCTION blind_offset_existence_on_observation_existence()
RETURNS TRIGGER AS $$
BEGIN
  IF (NEW.c_existence != OLD.c_existence) THEN
    UPDATE t_target 
    SET c_existence = NEW.c_existence
    WHERE c_target_id = NEW.c_blind_offset_target_id;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

--------------------------------------------------------
-- When a blind offset target is modified, invalidate obscalc and update the 
-- c_explicit_blind_offset flag on the observation
--------------------------------------------------------
CREATE OR REPLACE FUNCTION blind_offset_target_update()
  RETURNS trigger AS $$
DECLARE
  obsid d_observation_id;
BEGIN
  SELECT c_observation_id INTO obsid
  FROM t_observation
  WHERE c_blind_offset_target_id = NEW.c_target_id;

  IF FOUND THEN
    CALL invalidate_obscalc(obsid);

    -- The blind offset is only edited by the user, which changes it to explicit
    UPDATE t_observation
    SET c_explicit_blind_offset = TRUE
    WHERE c_observation_id = obsid;

    -- Notify the change
    PERFORM pg_notify('ch_observation_edit', obsid || ',' || NEW.c_program_id  || ',' || 'UPDATE');
  END IF;
  RETURN NEW; 
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER trg_blind_offset_target_update
  AFTER UPDATE ON t_target
  FOR EACH ROW
  WHEN (NEW.c_target_disposition = 'blind_offset')
  EXECUTE FUNCTION blind_offset_target_update();

--------------------------------------------------------
-- When a blind offset target is deleted, the FK constraint on t_observation will set the observation's
-- c_blind_offset_target_id to NULL. We also want to set c_explicit_blind_offset to FALSE
--------------------------------------------------------
CREATE OR REPLACE FUNCTION blind_offset_target_delete()
  RETURNS trigger AS $$
BEGIN
  UPDATE t_observation
    SET c_explicit_blind_offset = FALSE
    WHERE c_blind_offset_target_id = OLD.c_target_id;
  RETURN OLD;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_blind_offset_target_delete
  BEFORE DELETE ON t_target
  FOR EACH ROW
  WHEN (OLD.c_target_disposition = 'blind_offset')
  EXECUTE FUNCTION blind_offset_target_delete();

