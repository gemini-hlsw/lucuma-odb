-- GMOS MOS spectroscopy options are loaded as separate rows whose focal-plane unit
-- is a custom mask rather than a builtin aperture.
-- The builtin FPU column therefore becomes nullable:

ALTER TABLE t_spectroscopy_config_option_gmos_north ALTER COLUMN c_fpu DROP NOT NULL;
ALTER TABLE t_spectroscopy_config_option_gmos_south ALTER COLUMN c_fpu DROP NOT NULL;

-- Enforce the invariant: the builtin FPU is null iff the row's focal plane is
-- multislit (MOS).
CREATE OR REPLACE FUNCTION check_gmos_fpu_matches_focal_plane() RETURNS trigger AS $$
DECLARE fp d_tag;
BEGIN
  SELECT c_focal_plane INTO fp FROM t_spectroscopy_config_option
   WHERE c_instrument = NEW.c_instrument AND c_index = NEW.c_index;
  IF (NEW.c_fpu IS NULL) <> (fp = 'multiple_slit') THEN
    RAISE EXCEPTION 'GMOS builtin FPU must be null iff focal plane is multiple_slit (instrument=%, index=%)',
      NEW.c_instrument, NEW.c_index;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_gmos_north_fpu_focal_plane
  BEFORE INSERT OR UPDATE ON t_spectroscopy_config_option_gmos_north
  FOR EACH ROW EXECUTE FUNCTION check_gmos_fpu_matches_focal_plane();

CREATE TRIGGER trg_gmos_south_fpu_focal_plane
  BEFORE INSERT OR UPDATE ON t_spectroscopy_config_option_gmos_south
  FOR EACH ROW EXECUTE FUNCTION check_gmos_fpu_matches_focal_plane();
