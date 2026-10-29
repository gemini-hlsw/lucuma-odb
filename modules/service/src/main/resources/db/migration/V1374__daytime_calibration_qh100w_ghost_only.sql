-- The 100W Quartz Halogen lamp exists for GHOST only.  Earlier daytime
-- calibration sets for other instruments may hold it, so move them to 5W first.
UPDATE t_daytime_calibration_set s
SET    c_gcal_continuum = 'QuartzHalogen5'
FROM   t_daytime_calibration d
WHERE  d.c_observation_id = s.c_observation_id
AND    s.c_gcal_continuum = 'QuartzHalogen100'
AND    d.c_instrument <> 'Ghost';

CREATE FUNCTION check_daytime_calibration_qh100w()
RETURNS TRIGGER AS $$
BEGIN

  IF EXISTS (
    SELECT 1
    FROM   t_daytime_calibration_set s
    JOIN   t_daytime_calibration d USING (c_observation_id)
    WHERE  s.c_observation_id = NEW.c_observation_id
    AND    s.c_gcal_continuum = 'QuartzHalogen100'
    AND    d.c_instrument <> 'Ghost'
  ) THEN
    RAISE EXCEPTION
      'Observation % uses the 100W Quartz Halogen lamp, which is available for GHOST only',
      NEW.c_observation_id
      USING ERRCODE = 'check_violation';
  END IF;

  RETURN NEW;

END;
$$ LANGUAGE plpgsql;

-- Not a constraint trigger: updates defer all constraints, and an error at
-- commit escapes the service's check violation handling.
CREATE TRIGGER trigger_t_daytime_calibration_set_qh100w
  AFTER INSERT OR UPDATE OF c_gcal_continuum, c_observation_id ON t_daytime_calibration_set
  FOR EACH ROW EXECUTE FUNCTION check_daytime_calibration_qh100w();

-- Checked at commit, so an instrument change and its new sets land together.
CREATE CONSTRAINT TRIGGER trigger_t_daytime_calibration_qh100w
  AFTER UPDATE OF c_instrument ON t_daytime_calibration
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION check_daytime_calibration_qh100w();
