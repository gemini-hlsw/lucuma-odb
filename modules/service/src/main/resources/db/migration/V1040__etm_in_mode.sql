
-- Exposure Time Mode gets its own table.
CREATE TABLE t_exposure_time_mode (
  c_exposure_time_mode_id SERIAL           PRIMARY KEY,
  c_exposure_time_mode    e_exp_time_mode  NOT NULL,
  c_signal_to_noise       numeric(11, 3)   CHECK (c_signal_to_noise > 0::numeric),
  c_signal_to_noise_at    d_wavelength_pm,
  c_exposure_time         interval         CHECK (c_exposure_time  >= '0'::interval),
  c_exposure_count        integer          CHECK (c_exposure_count >= 0),
  CONSTRAINT t_exposure_time_mode_check CHECK (
    CASE c_exposure_time_mode
      WHEN 'signal_to_noise'::e_exp_time_mode THEN num_nulls(c_signal_to_noise, c_signal_to_noise_at) = 0
      WHEN 'time_and_count'::e_exp_time_mode  THEN num_nulls(c_signal_to_noise_at, c_exposure_time, c_exposure_count) = 0
      ELSE true
    END
  ),
  c_observation_id d_observation_id NOT NULL -- temporary for migration
);

-- Create a requirements ETM link table to track exposure time mode in the
-- requirements.
CREATE TABLE t_exposure_time_mode_link (
  c_observation_id        d_observation_id PRIMARY KEY REFERENCES     t_observation(c_observation_id) ON DELETE CASCADE,
  c_exposure_time_mode_id integer          UNIQUE NOT NULL REFERENCES t_exposure_time_mode(c_exposure_time_mode_id) ON DELETE CASCADE
);

-- Populate the ETMs from the existing requirements in t_observation
INSERT INTO t_exposure_time_mode (
  c_exposure_time_mode,
  c_signal_to_noise,
  c_signal_to_noise_at,
  c_exposure_time,
  c_exposure_count,
  c_observation_id
)
SELECT
  c_exp_time_mode,
  c_etm_signal_to_noise,
  c_etm_signal_to_noise_at,
  c_etm_exp_time,
  c_etm_exp_count,
  c_observation_id
FROM t_observation
WHERE c_exp_time_mode IS NOT NULL;

INSERT INTO t_exposure_time_mode_link (
  c_observation_id,
  c_exposure_time_mode_id
)
SELECT
  c_observation_id,
  c_exposure_time_mode_id
FROM t_exposure_time_mode;

ALTER TABLE t_exposure_time_mode
  DROP COLUMN c_observation_id;

DROP VIEW IF EXISTS v_observation;
DROP VIEW IF EXISTS v_generator_params;

-- Drop the now redundant columns in t_observation
ALTER TABLE t_observation
  DROP COLUMN c_exp_time_mode,
  DROP COLUMN c_etm_signal_to_noise,
  DROP COLUMN c_etm_signal_to_noise_at,
  DROP COLUMN c_etm_exp_time,
  DROP COLUMN c_etm_exp_count;

-- Add a trigger to clean up orphaned ETM rows (e.g., when an observation is
-- deleted the link table row deletion is cascaded but nothing removes the ETM
-- entry).

CREATE OR REPLACE FUNCTION delete_exposure_time_mode()
RETURNS TRIGGER
LANGUAGE plpgsql AS $$
BEGIN
  DELETE FROM t_exposure_time_mode
  WHERE c_exposure_time_mode_id = OLD.c_exposure_time_mode_id;
  RETURN NULL;
END;
$$;

CREATE TRIGGER delete_exposure_time_mode_trigger
AFTER DELETE ON t_exposure_time_mode_link
FOR EACH ROW
EXECUTE FUNCTION delete_exposure_time_mode();

-- We need a view on the exposure time mode for the embedded s/n, t&c.
CREATE VIEW v_exposure_time_mode AS
  SELECT e.*,
  CASE WHEN e.c_exposure_time_mode = 'signal_to_noise' THEN e.c_exposure_time_mode_id END AS c_signal_to_noise_id,
  CASE WHEN e.c_exposure_time_mode = 'time_and_count'  THEN e.c_exposure_time_mode_id END AS c_time_and_count_id
FROM t_exposure_time_mode e;

-- Update the observation view to remove the etm id columns.
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
  e.c_exposure_time_mode,
  e.c_signal_to_noise,
  e.c_signal_to_noise_at,
  e.c_exposure_time,
  e.c_exposure_count,
  o.c_observing_mode_type,
  o.c_science_band,
  o.c_declared_complete,
  o.c_acq_reset_time,
  t.c_target_id,
  t.c_sid_rv,
  t.c_source_profile,
  t.c_target_disposition
FROM
  t_observation o
LEFT JOIN LATERAL (
  SELECT t.c_target_id,
         t.c_sid_rv,
         t.c_source_profile,
         t.c_target_disposition
    FROM t_asterism_target a
    INNER JOIN t_target t
      ON a.c_target_id = t.c_target_id
     AND t.c_existence = 'present'
   WHERE a.c_observation_id = o.c_observation_id
) t ON TRUE
LEFT JOIN t_exposure_time_mode_link k USING (c_observation_id)
LEFT JOIN t_exposure_time_mode      e ON k.c_exposure_time_mode_id = e.c_exposure_time_mode_id
ORDER BY
  o.c_observation_id,
  t.c_target_id;


-- Update the signal-to-noise type to match the exposure time mode
ALTER TABLE t_obscalc
  ALTER COLUMN c_img_single_sn  SET DATA TYPE numeric(11,3),
  ALTER COLUMN c_img_total_sn   SET DATA TYPE numeric(11,3),
  ALTER COLUMN c_spec_single_sn SET DATA TYPE numeric(11,3),
  ALTER COLUMN c_spec_total_sn  SET DATA TYPE numeric(11,3);