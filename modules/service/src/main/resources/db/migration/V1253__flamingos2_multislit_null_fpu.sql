-- Flamingos 2 MOS spectroscopy options are loaded as separate rows whose focal-plane unit
-- is a custom mask rather than a builtin slit.
-- The builtin FPU column therefore becomes nullable:

ALTER TABLE t_spectroscopy_config_option_f2 ALTER COLUMN c_fpu DROP NOT NULL;

-- Make the gmos check common for flamingos2 too.
ALTER FUNCTION check_gmos_fpu_matches_focal_plane() RENAME TO check_builtin_fpu_matches_focal_plane;

-- The message no longer names GMOS now that the check also covers F2.
CREATE OR REPLACE FUNCTION check_builtin_fpu_matches_focal_plane() RETURNS trigger AS $$
DECLARE fp d_tag;
BEGIN
  SELECT c_focal_plane INTO fp FROM t_spectroscopy_config_option
   WHERE c_instrument = NEW.c_instrument AND c_index = NEW.c_index;
  IF (NEW.c_fpu IS NULL) <> (fp = 'multiple_slit') THEN
    RAISE EXCEPTION 'Builtin FPU must be null iff focal plane is multiple_slit (instrument=%, index=%)',
      NEW.c_instrument, NEW.c_index;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_f2_fpu_focal_plane
  BEFORE INSERT OR UPDATE ON t_spectroscopy_config_option_f2
  FOR EACH ROW EXECUTE FUNCTION check_builtin_fpu_matches_focal_plane();
