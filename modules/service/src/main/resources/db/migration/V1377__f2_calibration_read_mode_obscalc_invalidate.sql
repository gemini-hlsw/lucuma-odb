-- Flamingos 2 arcs and flats are now always taken in Bright read mode, and the
-- telluric of a MOS observation gains a nighttime flat (sc-11154).  Both are
-- computed in code but change the generated sequence and its time estimate, so
-- every Flamingos 2 spectroscopy obscalc result is stale.  Imaging has no arcs
-- or flats and is unaffected.
UPDATE t_obscalc c
   SET c_obscalc_state = 'pending'
  FROM t_observation o
 WHERE c.c_observation_id = o.c_observation_id
   AND o.c_observing_mode_type IN ('flamingos_2_long_slit', 'flamingos_2_mos');
