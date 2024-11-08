--
-- Object angular size estimate based on image quality and source profile.
--
CREATE OR REPLACE FUNCTION calculate_object_size(
  iq           numeric(2, 1),
  src_profile  jsonb
)
RETURNS d_angle_µas AS $$
DECLARE
  profile_type e_source_profile_type;
  fwhm         d_angle_µas;
  fwhm_arcsec  double precision;
  iq_arcsec    double precision;
  result       d_angle_µas;
BEGIN

  SELECT c_profile_type, c_fwhm
  INTO   profile_type, fwhm
  FROM extract_source_profile_summary(src_profile);

  CASE
    WHEN profile_type = 'point' THEN
      result := iq * 1000000;
    WHEN profile_type = 'uniform' THEN
      result := 648000000000;  -- 180 degrees
    ELSE
      iq_arcsec   := iq::double precision;
      fwhm_arcsec := fwhm::double precision / 1e6;
      result      := (sqrt(fwhm_arcsec * fwhm_arcsec + iq_arcsec * iq_arcsec) * 1e6)::d_angle_µas;
  END CASE;

  RETURN result;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

--
-- The effective width of the object, which is the object size estimate but
-- limited by the slit width.
--
CREATE OR REPLACE FUNCTION calculate_effective_width(
  iq          numeric(2, 1),
  slit_µas    d_angle_µas,
  src_profile jsonb
)
RETURNS d_angle_µas AS $$
DECLARE
  obj_size_µas d_angle_µas;
BEGIN
  SELECT calculate_object_size(iq, src_profile) INTO obj_size_µas;
  RETURN least(obj_size_µas, slit_µas);
END;
$$ LANGUAGE plpgsql IMMUTABLE;

--
-- GMOS longslit spectral binning calculation.
--
CREATE OR REPLACE FUNCTION calculate_gmos_long_slit_spectral_binning(
  iq            numeric(2, 1),
  slit_µas      d_angle_µas,
  dispersion_pm smallint,
  resolution    smallint,
  blaze_pm      smallint,
  src_profile   jsonb,
  sampling      double precision
)
RETURNS smallint AS $$
DECLARE
  eff_width_µas d_angle_µas;
  eff_res_as    double precision;
  n_pix         double precision;
  binning       smallint[];
  bin           smallint;
BEGIN
  SELECT calculate_effective_width(iq, slit_µas, src_profile) INTO eff_width_µas;
  eff_res_as := resolution::double precision / 2.0 / (eff_width_µas::double precision / 1000000.0);
  n_pix      := (blaze_pm::double precision / 1000.0) / eff_res_as / (dispersion_pm / 1000.0);

  SELECT ARRAY(SELECT c_count FROM t_gmos_binning ORDER BY c_count DESC) INTO binning;
  FOREACH bin IN ARRAY binning
  LOOP
    IF sampling < (n_pix / bin::double precision) THEN
      RETURN bin;
    END IF;
  END LOOP;

  RETURN 1;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

--
-- GMOS longslit spatial binning calculation.
--
CREATE OR REPLACE FUNCTION calculate_gmos_long_slit_spatial_binning(
  iq              numeric(2, 1),
  pixel_scale_µas d_angle_µas,
  src_profile     jsonb,
  sampling        double precision
)
RETURNS smallint AS $$
DECLARE
  obj_size_µas  d_angle_µas;
  n_pix         double precision;
  binning       smallint[];
  bin           smallint;
BEGIN
  SELECT calculate_object_size(iq, src_profile) INTO obj_size_µas;
  n_pix := (obj_size_µas::double precision / 1000000.0) /
           (pixel_scale_µas::double precision / 1000000.0 * sampling);

  SELECT ARRAY(SELECT c_count FROM t_gmos_binning ORDER BY c_count DESC) INTO binning;
  FOREACH bin IN ARRAY binning
  LOOP
    IF bin::double precision < n_pix THEN
      RETURN bin;
    END IF;
  END LOOP;

  RETURN 1;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

CREATE OR REPLACE FUNCTION lookup_bin_tag(
  bin smallint
)
RETURNS d_tag AS $$
BEGIN
  RETURN (SELECT c_tag FROM t_gmos_binning WHERE c_count = bin);
END;
$$ LANGUAGE plpgsql IMMUTABLE;

--
-- (xbin, ybin) tuple
--
CREATE TYPE gmos_binning AS (
  xbin d_tag,
  ybin d_tag
);

-- Set the default binning.
CREATE OR REPLACE PROCEDURE set_gmos_long_slit_default_binning(
  oid d_observation_id
) AS $$
DECLARE
  iq              numeric(2, 1);
  mode            e_observing_mode_type;
  pixel_scale_µas d_angle_µas;
  slit_µas        d_angle_µas;
  dispersion_pm   smallint;
  resolution      smallint;
  blaze_pm        smallint;
  src_profile     jsonb;
  profiles        jsonb[];
  current_xbin    smallint;
  current_ybin    smallint;
  min_xbin        smallint := NULL;
  min_ybin        smallint := NULL;
  xbin            d_tag;
  ybin            d_tag;
BEGIN

  SELECT q.c_value, o.c_observing_mode_type
  INTO iq, mode
  FROM t_observation o
  LEFT JOIN t_image_quality q on o.c_image_quality = q.c_tag
  WHERE o.c_observation_id = oid;

  CASE
    WHEN mode = 'gmos_north_long_slit' THEN
      SELECT c_pixel_size FROM t_gmos_north_detector WHERE c_tag = 'HAMAMATSU' INTO pixel_scale_µas;

      SELECT f.c_slit_width, d.c_dispersion_pm, d.c_reference_resolution, d.c_blaze_wavelength_nm
      INTO slit_µas, dispersion_pm, resolution, blaze_pm
      FROM t_gmos_north_long_slit g
      LEFT JOIN t_gmos_north_fpu       f ON g.c_fpu     = f.c_tag
      LEFT JOIN t_gmos_north_disperser d ON g.c_grating = d.c_tag
      WHERE g.c_observation_id = oid;

    WHEN mode = 'gmos_south_long_slit' THEN
      SELECT c_pixel_size FROM t_gmos_south_detector WHERE c_tag = 'HAMAMATSU' INTO pixel_scale_µas;

      SELECT f.c_slit_width, d.c_dispersion_pm, d.c_reference_resolution, d.c_blaze_wavelength_nm
      INTO slit_µas, dispersion_pm, resolution, blaze_pm
      FROM t_gmos_south_long_slit g
      LEFT JOIN t_gmos_south_fpu       f ON g.c_fpu     = f.c_tag
      LEFT JOIN t_gmos_north_disperser d ON g.c_grating = d.c_tag
      WHERE g.c_observation_id = oid;

    ELSE
      RETURN;
  END CASE;

  SELECT ARRAY(
    SELECT t.c_source_profile
    FROM t_asterism_target a
    LEFT JOIN t_target t ON a.c_target_id = t.c_target_id
    WHERE c_observation_id = oid
  ) INTO profiles;

  -- HERE we want to compute the spectral (xbin) and spatial (ybin) binning for
  -- each profile and take the minimum value in each case.
  FOREACH src_profile IN ARRAY profiles
  LOOP
    -- Compute spectral (xbin) and spatial (ybin) binning for each profile
    current_xbin := calculate_gmos_long_slit_spectral_binning(
      iq, slit_µas, dispersion_pm, resolution, blaze_pm, src_profile, sampling := 2.5
    );
    current_ybin := calculate_gmos_long_slit_spatial_binning(
      iq, pixel_scale_µas, src_profile, sampling := 2.5
    );

    -- Track minimum xbin and ybin values
    min_xbin := LEAST(COALESCE(min_xbin, current_xbin), current_xbin);
    min_ybin := LEAST(COALESCE(min_ybin, current_ybin), current_ybin);
  END LOOP;

  xbin := lookup_bin_tag(min_xbin);
  ybin := lookup_bin_tag(min_ybin);

  RAISE NOTICE 'xbin %, ybin %', xbin, ybin;

  EXECUTE format('UPDATE t_%I SET c_xbin_default = $1, c_ybin_default = $2 WHERE c_observation_id = $3', mode::text)
  USING xbin, ybin, oid;
END;
$$ LANGUAGE plpgsql;