-- Flamingos 2 calibration sets may be DAYTIME_OBSERVE: an image with no lamp.
-- The list mirrors InstrumentSetup.allowedKinds.

CREATE OR REPLACE FUNCTION check_flamingos_2_daytime_calibration_set_kind()
  RETURNS trigger AS $$
DECLARE
  kind e_calibration_set_type;
BEGIN
  SELECT s.c_kind INTO kind
    FROM t_daytime_calibration_set s
    JOIN t_flamingos_2_daytime_calibration_setup f ON f.c_set_id = s.c_set_id
   WHERE s.c_set_id = NEW.c_set_id;
  IF FOUND AND kind NOT IN ('dark', 'flat', 'arc', 'daytime_observe', 'smart_flat', 'smart_arc', 'day_baseline') THEN
    RAISE EXCEPTION 'A Flamingos 2 calibration set must be a DARK, FLAT, ARC, DAYTIME_OBSERVE, SMART_FLAT, SMART_ARC or DAY_BASELINE, not %', kind;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
