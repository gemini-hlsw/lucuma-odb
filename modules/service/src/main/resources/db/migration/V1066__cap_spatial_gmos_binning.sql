-- Cap spatial binning to 2: https://app.shortcut.com/lucuma/story/7020/update-gmos-default-y-binning-calculation

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

        -- Spatial binning capped at 2.
        IF current_ybin > 2 THEN
          current_ybin := 2;
        END IF;

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
