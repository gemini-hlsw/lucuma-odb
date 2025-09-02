-- Add default binning to gmos imaging
ALTER TABLE t_gmos_north_imaging
  ADD COLUMN c_bin_default d_tag NOT NULL REFERENCES t_gmos_binning(c_tag) DEFAULT 'Two'::d_tag;

ALTER TABLE t_gmos_south_imaging
  ADD COLUMN c_bin_default d_tag NOT NULL REFERENCES t_gmos_binning(c_tag) DEFAULT 'Two'::d_tag;

-- Recreate the views to add default binning
DROP VIEW IF EXISTS v_gmos_north_imaging;
CREATE VIEW v_gmos_north_imaging AS
  SELECT
    i.*,
    f.c_filters,
    fs.c_initial_filters
  FROM
    t_gmos_north_imaging i
  INNER JOIN v_gmos_north_imaging_filter f
    ON i.c_observation_id = f.c_observation_id
  INNER JOIN v_gmos_north_imaging_initial_filter fs
    ON i.c_observation_id = fs.c_observation_id;

DROP VIEW IF EXISTS v_gmos_south_imaging;
CREATE VIEW v_gmos_south_imaging AS
  SELECT
    i.*,
    f.c_filters,
    fs.c_initial_filters
  FROM
    t_gmos_south_imaging i
  INNER JOIN v_gmos_south_imaging_filter f
    ON i.c_observation_id = f.c_observation_id
  INNER JOIN v_gmos_south_imaging_initial_filter fs
    ON i.c_observation_id = fs.c_observation_id;

-- There is nothing mode-specific about the spectral / spatial binning functions
-- so rename them in order to reuse them for imaging mode.

ALTER FUNCTION public.calculate_gmos_long_slit_spectral_binning
  RENAME TO calculate_gmos_spectral_binning;

ALTER FUNCTION public.calculate_gmos_long_slit_spatial_binning
  RENAME to calculate_gmos_spatial_binning;

ALTER PROCEDURE public.set_gmos_long_slit_default_binning
  RENAME to set_gmos_default_binning;

-- Update the default binning call site and include imaging.
CREATE OR REPLACE PROCEDURE set_gmos_default_binning(
  oid d_observation_id
) AS $$
DECLARE
  iq              numeric(2, 1);
  mode            e_observing_mode_type;
  pixel_scale_µas d_angle_µas;
  slit_µas        d_angle_µas;
  dispersion_pm   smallint;
  resolution      smallint;
  blaze_nm        smallint;
  src_profile     jsonb;
  profiles        jsonb[];
  current_xbin    smallint;
  current_ybin    smallint;
  min_xbin        smallint := 100;
  min_ybin        smallint := 100;
  xbin            d_tag;
  ybin            d_tag;
BEGIN

  -- Determine the IQ and observing mode.
  SELECT q.c_value, o.c_observing_mode_type
  INTO iq, mode
  FROM t_observation o
  LEFT JOIN t_image_quality q on o.c_image_quality = q.c_tag
  WHERE o.c_observation_id = oid;

  -- Lookup the slit width, dispersion, resolution and blaze wavelength.
  CASE
    WHEN mode = 'gmos_north_imaging' THEN
      SELECT c_pixel_size FROM t_gmos_north_detector WHERE c_tag = 'HAMAMATSU' INTO pixel_scale_µas;

    WHEN mode = 'gmos_north_long_slit' THEN
      SELECT c_pixel_size FROM t_gmos_north_detector WHERE c_tag = 'HAMAMATSU' INTO pixel_scale_µas;

      SELECT f.c_slit_width, d.c_dispersion_pm, d.c_reference_resolution, d.c_blaze_wavelength_nm
      INTO slit_µas, dispersion_pm, resolution, blaze_nm
      FROM t_gmos_north_long_slit g
      LEFT JOIN t_gmos_north_fpu       f ON g.c_fpu     = f.c_tag
      LEFT JOIN t_gmos_north_disperser d ON g.c_grating = d.c_tag
      WHERE g.c_observation_id = oid;

    WHEN mode = 'gmos_south_imaging' THEN
      SELECT c_pixel_size FROM t_gmos_south_detector WHERE c_tag = 'HAMAMATSU' INTO pixel_scale_µas;

    WHEN mode = 'gmos_south_long_slit' THEN
      SELECT c_pixel_size FROM t_gmos_south_detector WHERE c_tag = 'HAMAMATSU' INTO pixel_scale_µas;

      SELECT f.c_slit_width, d.c_dispersion_pm, d.c_reference_resolution, d.c_blaze_wavelength_nm
      INTO slit_µas, dispersion_pm, resolution, blaze_nm
      FROM t_gmos_south_long_slit g
      LEFT JOIN t_gmos_south_fpu       f ON g.c_fpu     = f.c_tag
      LEFT JOIN t_gmos_south_disperser d ON g.c_grating = d.c_tag
      WHERE g.c_observation_id = oid;

    ELSE
      RETURN;
  END CASE;

  -- Get all the source profiles in the asterism.
  SELECT ARRAY(
    SELECT t.c_source_profile
    FROM t_asterism_target a
    LEFT JOIN t_target t ON a.c_target_id = t.c_target_id
    WHERE a.c_observation_id = oid
  ) INTO profiles;

  -- HERE we want to compute the spectral (xbin) and spatial (ybin) binning for
  -- each profile and take the minimum value in each case.
  FOREACH src_profile IN ARRAY profiles
  LOOP
    -- Compute spectral (xbin) and spatial (ybin) binning for each profile
    CASE
      WHEN mode = 'gmos_north_imaging' OR mode = 'gmos_south_imaging' THEN
        current_xbin := calculate_gmos_spatial_binning(
          iq, pixel_scale_µas, src_profile, sampling := 2.5
        );

        -- Imaging binning capped at 2.
        IF current_xbin > 2 THEN
          current_xbin := 2;
        END IF;

        current_ybin := current_xbin;

      WHEN mode = 'gmos_north_long_slit' OR mode = 'gmos_south_long_slit' THEN
        current_xbin := calculate_gmos_spectral_binning(
          iq, slit_µas, dispersion_pm, resolution, blaze_nm, src_profile, sampling := 2.5
        );
        current_ybin := calculate_gmos_spatial_binning(
          iq, pixel_scale_µas, src_profile, sampling := 2.5
        );

      ELSE
        RETURN;
    END CASE;

    -- Track minimum xbin and ybin values
    min_xbin := LEAST(COALESCE(min_xbin, current_xbin), current_xbin);
    min_ybin := LEAST(COALESCE(min_ybin, current_ybin), current_ybin);
  END LOOP;

  -- Turn the binning number into a d_tag, if possible.
  xbin := lookup_bin_tag(min_xbin);
  ybin := lookup_bin_tag(min_ybin);

  -- Set the binning in the appropriate long table (north or south).
  IF xbin IS NOT NULL AND ybin IS NOT NULL THEN
    CASE
      WHEN mode = 'gmos_north_imaging' OR mode = 'gmos_south_imaging' THEN
        EXECUTE format('UPDATE t_%I SET c_bin_default = $1 WHERE c_observation_id = $2', mode::text)
        USING ybin, oid;

      WHEN mode = 'gmos_north_long_slit' OR mode = 'gmos_south_long_slit' THEN
        EXECUTE format('UPDATE t_%I SET c_xbin_default = $1, c_ybin_default = $2 WHERE c_observation_id = $3', mode::text)
        USING xbin, ybin, oid;

      ELSE
        RETURN;
    END CASE;
  END IF;
END;
$$ LANGUAGE plpgsql;

-- Update the triggers to call the new set default binning procedure.
DROP TRIGGER gmos_north_long_slit_binning_trigger ON t_gmos_north_long_slit;
DROP TRIGGER gmos_south_long_slit_binning_trigger ON t_gmos_south_long_slit;
DROP TRIGGER asterism_target_binning_trigger ON t_asterism_target;
DROP TRIGGER observation_binning_trigger ON t_observation;
DROP FUNCTION trigger_set_gmos_long_slit_default_binning;

CREATE OR REPLACE FUNCTION trigger_set_gmos_default_binning()
RETURNS TRIGGER AS $$
DECLARE
  oid d_observation_id;
BEGIN
  SELECT COALESCE(NEW.c_observation_id, OLD.c_observation_id) INTO oid;
  CALL set_gmos_default_binning(oid);
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER gmos_north_long_slit_binning_trigger
AFTER INSERT OR DELETE OR UPDATE OF c_fpu, c_grating
ON t_gmos_north_long_slit
FOR EACH ROW
EXECUTE FUNCTION trigger_set_gmos_default_binning();

CREATE TRIGGER gmos_south_long_slit_binning_trigger
AFTER INSERT OR DELETE OR UPDATE OF c_fpu, c_grating
ON t_gmos_south_long_slit
FOR EACH ROW
EXECUTE FUNCTION trigger_set_gmos_default_binning();

CREATE TRIGGER asterism_target_binning_trigger
AFTER INSERT OR DELETE OR UPDATE
ON t_asterism_target
FOR EACH ROW
EXECUTE FUNCTION trigger_set_gmos_default_binning();

CREATE TRIGGER observation_binning_trigger
AFTER INSERT OR UPDATE OF c_image_quality
ON t_observation
FOR EACH ROW
EXECUTE FUNCTION trigger_set_gmos_default_binning();

-- N.B., there are no triggers specific to the imaging table itself, because the
-- spatial binning calculation doesn't depend on instrument features but rather
-- on the source profile and image quality which are covered in the last two
-- triggers above.