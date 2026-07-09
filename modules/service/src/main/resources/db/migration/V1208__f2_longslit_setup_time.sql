-- longslit acquisition time is now 10 min instead of 20
UPDATE t_time_estimate
   SET c_time = '10 minutes'
 WHERE c_tag = 'f2_longslit_setup';

-- The setup time estimate feeds into obscalc, so previously-computed Flamingos 2
-- results are stale and must be recalculated. 
UPDATE t_obscalc c
   SET c_obscalc_state = 'pending'
  FROM t_observation o
 WHERE c.c_observation_id = o.c_observation_id
   AND o.c_instrument = 'Flamingos2'
   AND c.c_workflow_state NOT IN ('inactive', 'ongoing', 'completed');
