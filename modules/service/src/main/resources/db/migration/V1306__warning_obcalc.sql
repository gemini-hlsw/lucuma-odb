
-- force an obscalc update when the dismissed warning set changes

CREATE OR REPLACE FUNCTION warning_dismissal_obscalc_invalidate()
  RETURNS TRIGGER AS $$
BEGIN
  IF NEW.c_dismissed_warnings IS DISTINCT FROM OLD.c_dismissed_warnings THEN
    CALL invalidate_all_obscalc_for_program(NEW.c_program_id);
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER warning_dismissal_invalidate_obscalc_trigger
  AFTER UPDATE ON t_program
  FOR EACH ROW
  EXECUTE FUNCTION warning_dismissal_obscalc_invalidate();

