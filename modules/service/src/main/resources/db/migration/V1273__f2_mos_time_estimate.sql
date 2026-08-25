-- Dedicated setup time estimates for the Flamingos 2 MOS observing mode.

INSERT INTO t_time_estimate VALUES(
  'f2_mos_setup',
  'Flamingos 2 MOS Setup',
  'Flamingos 2 MOS mode full setup cost',
  'Flamingos2',
  '30 minutes'
);

INSERT INTO t_time_estimate VALUES(
  'f2_mos_max_visit',
  'Flamingos 2 MOS Max Visit',
  'Flamingos 2 MOS Max Visit',
  'Flamingos2',
  '2 hours'
);

-- MOS setup was the long slit's 10 minutes until now, so every F2 MOS digest is stale.
UPDATE t_obscalc c
   SET c_obscalc_state = 'pending'
  FROM t_observation o
 WHERE c.c_observation_id = o.c_observation_id
   AND o.c_observing_mode_type = 'flamingos_2_mos';
