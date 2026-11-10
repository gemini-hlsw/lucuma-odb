-- A Flamingos 2 MOS acquisition now takes its sky step before the on-target
-- step, and its Fine Adjustments atoms take no sky step (sc-11452). Both are
-- computed in code but change the generated sequence and its time estimate, so
-- every Flamingos 2 MOS obscalc result is stale.
UPDATE t_obscalc c
   SET c_obscalc_state = 'pending'
  FROM t_observation o
 WHERE c.c_observation_id = o.c_observation_id
   AND o.c_observing_mode_type = 'flamingos_2_mos';
