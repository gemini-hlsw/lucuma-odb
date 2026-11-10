INSERT INTO t_gmos_north_fpu (c_tag, c_short_name, c_long_name, c_slit_width, c_x_offset)
VALUES ('Pinhole', 'Pinhole', 'GMOS-N Pinhole Mask', 0, 0);

INSERT INTO t_gmos_south_fpu (c_tag, c_short_name, c_long_name, c_slit_width, c_x_offset)
VALUES ('Pinhole', 'Pinhole', 'GMOS-S Pinhole Mask', 0, 0);

-- The pinhole has a zero slit width.  Return a binning of 1 instead of dividing
-- by zero, matching the lucuma-core Scala calculation.
CREATE OR REPLACE FUNCTION calculate_gmos_spectral_binning(
  iq            numeric(2, 1),
  slit_µas      d_angle_µas,
  dispersion_pm smallint,
  resolution    smallint,
  blaze_nm      smallint,
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

  IF eff_width_µas = 0 THEN
    RETURN 1;
  END IF;

  eff_res_as := resolution::double precision / 2.0 / (eff_width_µas::double precision / 1000000.0);
  n_pix      := blaze_nm::double precision / eff_res_as / (dispersion_pm::double precision / 1000.0);

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
