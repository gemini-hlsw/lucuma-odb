-- Prevent editing targets on calibrations other than telluric

CREATE OR REPLACE FUNCTION prevent_calibration_target_edit()
RETURNS TRIGGER AS $$
DECLARE
  obs_count INTEGER;
BEGIN
  -- Check if target is in any observation with a calibration role
  SELECT COUNT(*) INTO obs_count
  FROM t_asterism_target a
  JOIN t_observation o ON a.c_observation_id = o.c_observation_id
  WHERE a.c_target_id = NEW.c_target_id
    AND o.c_calibration_role IS NOT NULL
    AND o.c_calibration_role <> 'telluric';

  IF obs_count > 0 THEN
    RAISE EXCEPTION 'Target % cannot be edited because it is in a calibration observation', NEW.c_target_id;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER prevent_calibration_target_edit_trigger
BEFORE UPDATE ON t_target
FOR EACH ROW
EXECUTE FUNCTION prevent_calibration_target_edit();
