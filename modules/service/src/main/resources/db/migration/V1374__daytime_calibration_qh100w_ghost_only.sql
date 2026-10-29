-- The 100W Quartz Halogen lamp exists for GHOST only.  Earlier daytime
-- calibration sets for other instruments may hold it, so move them to 5W.
-- Sets with a stored sequence are frozen, so those stay as they are.
UPDATE t_daytime_calibration_set s
SET    c_gcal_continuum = 'QuartzHalogen5'
FROM   t_daytime_calibration d
WHERE  d.c_observation_id = s.c_observation_id
AND    s.c_gcal_continuum = 'QuartzHalogen100'
AND    d.c_instrument <> 'Ghost'
AND    NOT EXISTS (
  SELECT 1
  FROM   t_sequence_materialization m
  WHERE  m.c_observation_id = s.c_observation_id
  AND    m.c_sequence_type  = 'science'
);
