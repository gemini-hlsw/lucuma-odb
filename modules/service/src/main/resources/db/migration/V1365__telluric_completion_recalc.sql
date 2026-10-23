-- A visit ends when its After telluric completes, and only then is the next
-- visit's set of tellurics built. Completing a telluric only marked the science
-- calibration estimate stale, and that refresh does not bump c_last_update, so
-- nothing re-ran the calibration recalculation. Enqueue it when a telluric's
-- science sequence completes, whether computed or declared.

CREATE OR REPLACE FUNCTION telluric_completion_recalc(telluric_id d_observation_id)
RETURNS void AS $$
DECLARE
  science_id d_observation_id;
  prog_id    d_program_id;
BEGIN
  SELECT r.c_science_observation_id, r.c_program_id
  INTO   science_id, prog_id
  FROM   t_telluric_resolution r
  WHERE  r.c_observation_id = telluric_id;
  IF FOUND THEN
    CALL invalidate_calibration_calc(science_id, prog_id, 'recalc');
  END IF;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION telluric_obscalc_completion_recalc()
RETURNS TRIGGER AS $$
BEGIN
  IF NEW.c_sci_execution_state = 'completed'
     AND OLD.c_sci_execution_state IS DISTINCT FROM 'completed' THEN
    PERFORM telluric_completion_recalc(NEW.c_observation_id);
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER telluric_obscalc_completion_recalc_trigger
  AFTER UPDATE OF c_sci_execution_state ON t_obscalc
  FOR EACH ROW
  EXECUTE FUNCTION telluric_obscalc_completion_recalc();

CREATE OR REPLACE FUNCTION telluric_declared_completion_recalc()
RETURNS TRIGGER AS $$
BEGIN
  IF NEW.c_declared_state = 'declared_complete'
     AND OLD.c_declared_state IS DISTINCT FROM 'declared_complete'
     AND NEW.c_calibration_role = 'telluric' THEN
    PERFORM telluric_completion_recalc(NEW.c_observation_id);
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER telluric_declared_completion_recalc_trigger
  AFTER UPDATE OF c_declared_state ON t_observation
  FOR EACH ROW
  EXECUTE FUNCTION telluric_declared_completion_recalc();
