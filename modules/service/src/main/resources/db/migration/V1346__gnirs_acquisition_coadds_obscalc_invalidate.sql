-- GNIRS signal-to-noise acquisitions now raise the coadds of the steps after the
-- FPU image towards a 5 second integration (sc-10747).  That is computed in code,
-- but it changes the generated sequence and its time estimate, so every GNIRS
-- obscalc result computed with a signal-to-noise acquisition is stale.  A
-- time-and-count acquisition keeps its explicit coadds and is unaffected.
UPDATE t_obscalc c
   SET c_obscalc_state = 'pending'
  FROM t_observation o
  JOIN t_exposure_time_mode e
    ON e.c_observation_id = o.c_observation_id
   AND e.c_role           = 'acquisition'
 WHERE c.c_observation_id = o.c_observation_id
   AND o.c_instrument          = 'Gnirs'
   AND e.c_exposure_time_mode  = 'signal_to_noise';
