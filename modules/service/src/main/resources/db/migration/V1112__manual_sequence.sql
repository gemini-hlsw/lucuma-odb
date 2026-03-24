-- For normal observations, if the sequence has been materialized we don't want
-- the observation to be hard deleted.  This is why we do not cascade deletes in
-- the FK reference to t_observation(c_observation_id).  If it is a calibration
-- though, we'll let it clean up after itself if the sequence was edited but the
-- observation was never executed.

CREATE OR REPLACE FUNCTION delete_sequence_materialization_if_calibration()
RETURNS trigger AS $$
BEGIN
  IF OLD.c_calibration_role IS NOT NULL THEN
    DELETE FROM t_sequence_materialization
    WHERE c_observation_id = OLD.c_observation_id;
  END IF;

  RETURN OLD;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER delete_sequence_materialization_if_calibration_trigger
BEFORE DELETE ON t_observation
FOR EACH ROW
EXECUTE FUNCTION delete_sequence_materialization_if_calibration();