-- Update the grating tables to add the wavelength dither for each option.

ALTER TABLE t_gmos_north_disperser
  ADD COLUMN c_wavelength_dither_nm smallint CHECK (c_wavelength_dither_nm > 0);

ALTER TABLE t_gmos_south_disperser
  ADD COLUMN c_wavelength_dither_nm smallint CHECK (c_wavelength_dither_nm > 0);

UPDATE t_gmos_north_disperser
  SET c_wavelength_dither_nm =
    CASE
      WHEN c_tag = 'B1200_G5301' THEN  5
      WHEN c_tag = 'R831_G5302'  THEN  5
      WHEN c_tag = 'B600_G5303'  THEN  5
      WHEN c_tag = 'B600_G5307'  THEN  5
      WHEN c_tag = 'R600_G5304'  THEN  5
      WHEN c_tag = 'B480_G5309'  THEN  5
      WHEN c_tag = 'R400_G5305'  THEN  8
      WHEN c_tag = 'R150_G5306'  THEN 20
      WHEN c_tag = 'R150_G5308'  THEN 20
    END;

UPDATE t_gmos_south_disperser
  SET c_wavelength_dither_nm =
    CASE
      WHEN c_tag = 'B1200_G5321' THEN  5
      WHEN c_tag = 'R831_G5322'  THEN  5
      WHEN c_tag = 'B600_G5323'  THEN  5
      WHEN c_tag = 'B480_G5327'  THEN  5
      WHEN c_tag = 'R400_G5325'  THEN  8
      WHEN c_tag = 'R150_G5326'  THEN 20
      WHEN c_tag = 'R600_G5324'  THEN  5
    END;

ALTER TABLE t_gmos_north_disperser
  ALTER COLUMN c_wavelength_dither_nm SET NOT NULL;

ALTER TABLE t_gmos_south_disperser
  ALTER COLUMN c_wavelength_dither_nm SET NOT NULL;

-- Create a function to format the default wavelength dither
CREATE OR REPLACE FUNCTION gmos_long_slit_default_wavelength_dither(
  dither SMALLINT
) RETURNS TEXT AS $$
BEGIN
  RETURN '0,' || dither || ',' || -dither;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- Add a default x and y bin column to each mode.
ALTER TABLE t_gmos_north_long_slit
  ADD COLUMN c_xbin_default d_tag NOT NULL DEFAULT 'One'::d_tag,
  ADD COLUMN c_ybin_default d_tag NOT NULL DEFAULT 'One'::d_tag;

ALTER TABLE t_gmos_south_long_slit
  ADD COLUMN c_xbin_default d_tag NOT NULL DEFAULT 'One'::d_tag,
  ADD COLUMN c_ybin_default d_tag NOT NULL DEFAULT 'One'::d_tag;

-- A function that will format the GMOS long slit mode into a single text value
CREATE OR REPLACE FUNCTION format_gmos_long_slit_mode_group(
  site                  e_site,
  observing_mode_type   e_observing_mode_type,
  grating               d_tag,
  filter                d_tag,
  fpu                   d_tag,
  central_wavelength    d_wavelength_pm,
  xbin                  d_tag,
  xbin_default          d_tag,
  ybin                  d_tag,
  ybin_default          d_tag,
  amp_read_mode         d_tag,
  amp_gain              d_tag,
  roi                   d_tag,
  wavelength_dithers    text,
  spatial_offsets       text
) RETURNS text AS $$
DECLARE
  dither_nm smallint;
  formatted_spatial text;
BEGIN
  -- Figure out the default wavelength dither value.
  EXECUTE format(
    'SELECT c_wavelength_dither_nm FROM t_gmos_%I_disperser WHERE c_tag = $1',
    CASE
      WHEN site = 'gn' THEN 'north'
      WHEN site = 'gs' THEN 'south'
    END
  ) INTO dither_nm USING grating;

  -- Trim trailing zeros from the spatial offsets so that numerically equivalent
  -- values compare the same.
  IF spatial_offsets IS NOT NULL THEN
    formatted_spatial := regexp_replace(spatial_offsets, '(\.\d*?[1-9])0*|\.(0+)', '\1', 'g');
  ELSE
    formatted_spatial := '0,15,-15';
  END IF;

  -- Concat all the fields together into a text value.  Here we use the default
  -- value when an explicit override is not present.  For example, a mode that
  -- explicitly requests binning of 1x1 will compare the same as one that simply
  -- defaults to 1x1 based on the source profile and image quality, etc.
  RETURN concat_ws(
    ':',
    observing_mode_type::text,
    grating::text,
    filter::text,
    fpu::text,
    central_wavelength::text,
    COALESCE(xbin, xbin_default)::text,
    COALESCE(ybin, ybin_default)::text,
    COALESCE(amp_read_mode::text, 'Slow'),
    COALESCE(amp_gain::text, 'Low'),
    COALESCE(roi::text, 'FullFrame'),
    COALESCE(
      wavelength_dithers,
      gmos_long_slit_default_wavelength_dither(dither_nm)
    ),
    formatted_spatial
  );
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- Add a generated "mode key" column.  We group on this value to find
-- observations that share the same observing mode.
ALTER TABLE t_gmos_north_long_slit
  ADD COLUMN c_mode_key text NOT NULL GENERATED ALWAYS AS (
    format_gmos_long_slit_mode_group(
     'gn',
      c_observing_mode_type,
      c_grating,
      c_filter,
      c_fpu,
      c_central_wavelength,
      c_xbin,
      c_xbin_default,
      c_ybin,
      c_ybin_default,
      c_amp_read_mode,
      c_amp_gain,
      c_roi,
      c_wavelength_dithers,
      c_spatial_offsets
    )
  ) STORED;

ALTER TABLE t_gmos_south_long_slit
  ADD COLUMN c_mode_key text NOT NULL GENERATED ALWAYS AS (
    format_gmos_long_slit_mode_group(
     'gs',
      c_observing_mode_type,
      c_grating,
      c_filter,
      c_fpu,
      c_central_wavelength,
      c_xbin,
      c_xbin_default,
      c_ybin,
      c_ybin_default,
      c_amp_read_mode,
      c_amp_gain,
      c_roi,
      c_wavelength_dithers,
      c_spatial_offsets
    )
  ) STORED;

-- The observing mode group view contains an entry per distinct observing mode
-- and program combination across all observing modes.
CREATE VIEW v_observing_mode_group AS
  SELECT
    m.c_mode_key,
    o.c_program_id,
    max(m.c_observation_id) as c_observation_id -- arbitrary, just pick one
  FROM
    t_gmos_north_long_slit m
  LEFT JOIN t_observation o ON m.c_observation_id = o.c_observation_id
  GROUP BY
    m.c_mode_key,
    o.c_program_id
UNION ALL
  SELECT
    m.c_mode_key,
    o.c_program_id,
    max(m.c_observation_id) as c_observation_id -- arbitrary, just pick one
  FROM
    t_gmos_south_long_slit m
  LEFT JOIN t_observation o ON m.c_observation_id = o.c_observation_id
  GROUP BY
    m.c_mode_key,
    o.c_program_id;

-- Replace the observation view to add an observing mode key.
DROP VIEW v_observation;

CREATE OR REPLACE VIEW v_observation AS
  SELECT o.*,
  CASE WHEN o.c_explicit_ra              IS NOT NULL THEN o.c_observation_id END AS c_explicit_base_id,
  CASE WHEN o.c_air_mass_min             IS NOT NULL THEN o.c_observation_id END AS c_air_mass_id,
  CASE WHEN o.c_hour_angle_min           IS NOT NULL THEN o.c_observation_id END AS c_hour_angle_id,
  CASE WHEN o.c_observing_mode_type      IS NOT NULL THEN o.c_observation_id END AS c_observing_mode_id,
  CASE WHEN o.c_spec_wavelength          IS NOT NULL THEN o.c_observation_id END AS c_spec_wavelength_id,
  CASE WHEN o.c_spec_signal_to_noise_at  IS NOT NULL THEN o.c_observation_id END AS c_spec_signal_to_noise_at_id,
  CASE WHEN o.c_spec_wavelength_coverage IS NOT NULL THEN o.c_observation_id END AS c_spec_wavelength_coverage_id,
  CASE WHEN o.c_spec_focal_plane_angle   IS NOT NULL THEN o.c_observation_id END AS c_spec_focal_plane_angle_id,
  CASE WHEN o.c_observation_duration     IS NOT NULL THEN o.c_observation_id END AS c_observation_duration_id,
  c.c_active_start::timestamp + (c.c_active_end::timestamp - c.c_active_start::timestamp) * 0.5 AS c_reference_time,
  COALESCE(
    CASE WHEN o.c_observing_mode_type = 'gmos_north_long_slit' THEN mode_gnls.c_mode_key END,
    CASE WHEN o.c_observing_mode_type = 'gmos_south_long_slit' THEN mode_gsls.c_mode_key END,
    NULL
  ) AS c_mode_key
  FROM t_observation o
  LEFT JOIN t_proposal p on p.c_program_id = o.c_program_id
  LEFT JOIN t_cfp c on p.c_cfp_id = c.c_cfp_id
  LEFT JOIN t_gmos_north_long_slit mode_gnls ON o.c_observation_id = mode_gnls.c_observation_id
  LEFT JOIN t_gmos_south_long_slit mode_gsls ON o.c_observation_id = mode_gsls.c_observation_id;

-- Update these views to get the new default binning column
DROP VIEW v_gmos_north_long_slit;

CREATE OR REPLACE VIEW v_gmos_north_long_slit AS
SELECT
  m.*,
  o.c_image_quality,
  t.c_source_profile
FROM
  t_gmos_north_long_slit m
INNER JOIN t_observation o
  ON m.c_observation_id = o.c_observation_id
LEFT JOIN v_asterism_single_target_for_long_slit a
  ON m.c_observation_id = a.c_observation_id
LEFT JOIN t_target t
  ON a.c_target_id      = t.c_target_id;

DROP VIEW v_gmos_south_long_slit;

CREATE OR REPLACE VIEW v_gmos_south_long_slit AS
SELECT
  m.*,
  o.c_image_quality,
  t.c_source_profile
FROM
  t_gmos_south_long_slit m
INNER JOIN t_observation o
  ON m.c_observation_id = o.c_observation_id
LEFT JOIN v_asterism_single_target_for_long_slit a
  ON m.c_observation_id = a.c_observation_id
LEFT JOIN t_target t
  ON a.c_target_id      = t.c_target_id;

-- Set up triggers to maintain the default binning value.

CREATE OR REPLACE FUNCTION trigger_set_gmos_long_slit_default_binning()
RETURNS TRIGGER AS $$
DECLARE
  oid d_observation_id;
BEGIN
  SELECT COALESCE(NEW.c_observation_id, OLD.c_observation_id) INTO oid;
  CALL set_gmos_long_slit_default_binning(oid);
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER gmos_north_long_slit_binning_trigger
AFTER INSERT OR DELETE OR UPDATE OF c_fpu, c_grating
ON t_gmos_north_long_slit
FOR EACH ROW
EXECUTE FUNCTION trigger_set_gmos_long_slit_default_binning();

CREATE TRIGGER gmos_south_long_slit_binning_trigger
AFTER INSERT OR DELETE OR UPDATE OF c_fpu, c_grating
ON t_gmos_south_long_slit
FOR EACH ROW
EXECUTE FUNCTION trigger_set_gmos_long_slit_default_binning();

CREATE TRIGGER asterism_target_binning_trigger
AFTER INSERT OR DELETE
ON t_asterism_target
FOR EACH ROW
EXECUTE FUNCTION trigger_set_gmos_long_slit_default_binning();

CREATE TRIGGER observation_binning_trigger
AFTER INSERT OR UPDATE OF c_image_quality
ON t_observation
FOR EACH ROW
EXECUTE FUNCTION trigger_set_gmos_long_slit_default_binning();