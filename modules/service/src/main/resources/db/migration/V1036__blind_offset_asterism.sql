-- add blind offset flag and target
ALTER TABLE t_observation ADD COLUMN IF NOT EXISTS c_use_blind_offset boolean NOT NULL DEFAULT false;

-- Create a function to enforce blind offset uniqueness
CREATE OR REPLACE FUNCTION check_blind_offset_unique()
RETURNS TRIGGER AS $$
BEGIN
  IF EXISTS (
    SELECT 1 FROM t_target t
    WHERE t.c_target_id = NEW.c_target_id
    AND t.c_target_disposition = 'blind_offset'
  ) THEN
    -- Ensure no other blind offset target exists for this observation
    IF EXISTS (
      SELECT 1 FROM t_asterism_target a
      JOIN t_target t ON a.c_target_id = t.c_target_id
      WHERE a.c_observation_id = NEW.c_observation_id
      AND t.c_target_disposition = 'blind_offset'
      AND a.c_target_id != NEW.c_target_id
    ) THEN
      RAISE EXCEPTION 'Only one blind offset target allowed per observation';
    END IF;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Create trigger to enforce the constraint
CREATE TRIGGER trg_blind_offset_unique
  BEFORE INSERT OR UPDATE ON t_asterism_target
  FOR EACH ROW
  EXECUTE FUNCTION check_blind_offset_unique();

DROP VIEW IF EXISTS v_generator_params;
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
  c.c_active_start::timestamp + (c.c_active_end::timestamp - c.c_active_start::timestamp) * 0.5 AS c_reference_time,

  -- Include blind offset target from asterism
  (SELECT a.c_target_id
    FROM t_asterism_target a
    JOIN t_target t ON a.c_target_id = t.c_target_id
    WHERE a.c_observation_id = o.c_observation_id
    AND t.c_target_disposition = 'blind_offset'
    LIMIT 1) AS c_blind_offset_target_id

  FROM t_observation o
  LEFT JOIN t_proposal p on p.c_program_id = o.c_program_id
  LEFT JOIN t_cfp c on p.c_cfp_id = c.c_cfp_id
  LEFT JOIN t_gmos_north_long_slit mode_gnls ON o.c_observation_id = mode_gnls.c_observation_id
  LEFT JOIN t_gmos_south_long_slit mode_gsls ON o.c_observation_id = mode_gsls.c_observation_id
  LEFT JOIN t_gmos_north_imaging mode_gni ON o.c_observation_id = mode_gni.c_observation_id
  LEFT JOIN t_gmos_south_imaging mode_gsi ON o.c_observation_id = mode_gsi.c_observation_id;

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
  t.c_target_id,
  t.c_sid_rv,
  t.c_source_profile
FROM
  t_observation o
LEFT JOIN LATERAL (
  SELECT t.c_target_id,
         t.c_sid_rv,
         t.c_source_profile
    FROM t_asterism_target a
    INNER JOIN t_target t
      ON a.c_target_id = t.c_target_id
     AND t.c_existence = 'present'
   WHERE a.c_observation_id = o.c_observation_id
     -- Don't include blind offset for sequence generation
     AND (t.c_target_disposition = 'calibration'::e_target_disposition
       OR t.c_target_disposition = 'science'::e_target_disposition)
) t ON TRUE
ORDER BY
  o.c_observation_id,
  t.c_target_id;

-- Science observations use science targets, calibration observations use calibration targets
CREATE OR REPLACE FUNCTION asterism_update()
  RETURNS trigger AS $$
DECLARE
  obsid d_observation_id;
  is_calibration boolean;
BEGIN
  IF TG_OP = 'DELETE' THEN
    obsid := OLD.c_observation_id;
  ELSE
    obsid := NEW.c_observation_id;
  END IF;

  -- Check if this is a calibration observation
  SELECT (c_calibration_role IS NOT NULL) INTO is_calibration
  FROM t_observation WHERE c_observation_id = obsid;

  update t_observation a
  set c_asterism_group = coalesce(
    -- Asterism grouping: science targets for science obs, calibration targets for calibration obs
    (select to_json(array_agg(b.c_target_id order by b.c_target_id))::jsonb
    from t_asterism_target b
    join t_target t on b.c_target_id = t.c_target_id
    where a.c_observation_id = b.c_observation_id
     and (t.c_target_disposition = 'calibration'::e_target_disposition
       or t.c_target_disposition = 'science'::e_target_disposition)),
    '[]'::jsonb
  ), c_title = (
    -- Title generation: science targets for science obs, calibration targets for calibration obs
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
      and (t.c_target_disposition = 'calibration'::e_target_disposition
       or t.c_target_disposition = 'science'::e_target_disposition)
  )
  where a.c_observation_id = obsid;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
