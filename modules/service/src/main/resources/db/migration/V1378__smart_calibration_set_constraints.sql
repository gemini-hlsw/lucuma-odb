-- Smart calibration sets take their lamp configuration, exposure time and 
-- exposure count from the Smart GCAL lookup, so a smart row carries no count.
-- The constraints live apart from the enum change because a new enum value 
-- cannot be used in the transaction that adds it.

ALTER TABLE t_daytime_calibration_set
  DROP CONSTRAINT t_daytime_calibration_set_c_count_check,
  ALTER COLUMN c_count DROP NOT NULL,
  ADD CONSTRAINT t_daytime_calibration_set_count_check CHECK (
    CASE
      WHEN c_kind IN ('smart_flat', 'smart_arc', 'day_baseline') THEN c_count IS NULL
      ELSE c_count > 0
    END
  );

-- The GCAL check's ELSE arm already rejects a lamp configuration on every
-- kind other than flat and arc, so smart kinds need no change there.

CREATE OR REPLACE FUNCTION check_flamingos_2_daytime_calibration_set_kind()
  RETURNS trigger AS $$
DECLARE
  kind e_calibration_set_type;
BEGIN
  SELECT s.c_kind INTO kind
    FROM t_daytime_calibration_set s
    JOIN t_flamingos_2_daytime_calibration_setup f ON f.c_set_id = s.c_set_id
   WHERE s.c_set_id = NEW.c_set_id;
  IF FOUND AND kind NOT IN ('dark', 'flat', 'arc', 'smart_flat', 'smart_arc', 'day_baseline') THEN
    RAISE EXCEPTION 'A Flamingos 2 calibration set must be a DARK, FLAT, ARC, SMART_FLAT, SMART_ARC or DAY_BASELINE, not %', kind;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
