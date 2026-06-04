-- IGRINS-2 has on-instrument guiding and flexure compensation, so
-- re-acquisitions are not required.
-- Set the reacquisition overhead to 0.
UPDATE t_time_estimate
   SET c_time = '0 seconds'
 WHERE c_tag = 'igrins2_reacquisition';

-- invalidate all IGRINS-2 obscalc entries.
UPDATE t_obscalc c
   SET c_obscalc_state = 'pending'
  FROM t_observation o
 WHERE c.c_observation_id = o.c_observation_id
   AND o.c_instrument = 'Igrins2';
