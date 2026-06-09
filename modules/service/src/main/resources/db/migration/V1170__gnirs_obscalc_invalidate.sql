-- Several GNIRS acquisition sequence changes alter the generated acquisition
-- sequence (and therefore its time estimate), so previously-computed GNIRS obscalc
-- results are stale and must be recalculated. None of these require a schema change
-- (everything is computed in code), so this migration only invalidates obscalc.
UPDATE t_obscalc c
   SET c_obscalc_state = 'pending'
  FROM t_observation o
 WHERE c.c_observation_id = o.c_observation_id
   AND o.c_instrument = 'Gnirs';
