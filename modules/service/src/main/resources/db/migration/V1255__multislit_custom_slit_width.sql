-- MOS spectroscopy options use a custom mask rather than a builtin slit.  The phase 0 matrix
-- identifies the mask width, so record the enumerated value instead of leaving clients to
-- recover it from the row's angular slit width.  For Flamingos 2 that recovery is lossy, since
-- its widths are enumerated in detector pixels rather than arcseconds.

ALTER TABLE t_spectroscopy_config_option_f2
  ADD COLUMN c_custom_slit_width d_tag NULL REFERENCES t_f2_custom_slit_width(c_tag);

ALTER TABLE t_spectroscopy_config_option_gmos_north
  ADD COLUMN c_custom_slit_width d_tag NULL REFERENCES t_gmos_custom_slit_width(c_tag);

ALTER TABLE t_spectroscopy_config_option_gmos_south
  ADD COLUMN c_custom_slit_width d_tag NULL REFERENCES t_gmos_custom_slit_width(c_tag);

-- Mirror image of check_builtin_fpu_matches_focal_plane: the custom slit width is present
-- exactly when the builtin FPU is absent, making the pair a discriminated union.
CREATE OR REPLACE FUNCTION check_custom_slit_width_matches_focal_plane() RETURNS trigger AS $$
DECLARE fp d_tag;
BEGIN
  SELECT c_focal_plane INTO fp FROM t_spectroscopy_config_option
   WHERE c_instrument = NEW.c_instrument AND c_index = NEW.c_index;
  IF (NEW.c_custom_slit_width IS NOT NULL) <> (fp = 'multiple_slit') THEN
    RAISE EXCEPTION 'Custom slit width must be non-null iff focal plane is multiple_slit (instrument=%, index=%)',
      NEW.c_instrument, NEW.c_index;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_f2_custom_slit_width_focal_plane
  BEFORE INSERT OR UPDATE ON t_spectroscopy_config_option_f2
  FOR EACH ROW EXECUTE FUNCTION check_custom_slit_width_matches_focal_plane();

CREATE TRIGGER trg_gmos_north_custom_slit_width_focal_plane
  BEFORE INSERT OR UPDATE ON t_spectroscopy_config_option_gmos_north
  FOR EACH ROW EXECUTE FUNCTION check_custom_slit_width_matches_focal_plane();

CREATE TRIGGER trg_gmos_south_custom_slit_width_focal_plane
  BEFORE INSERT OR UPDATE ON t_spectroscopy_config_option_gmos_south
  FOR EACH ROW EXECUTE FUNCTION check_custom_slit_width_matches_focal_plane();
