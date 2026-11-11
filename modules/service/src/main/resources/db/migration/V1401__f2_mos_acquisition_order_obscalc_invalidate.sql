-- A Flamingos 2 MOS acquisition now takes its sky step before the on-target
-- step, and its Fine Adjustments atoms take no sky step (sc-11452). Both are
-- computed in code but change the generated sequence and its time estimate, so
-- every Flamingos 2 MOS obscalc result is stale. A deleted observation is
-- recalculated if it is restored, and one whose science sequence is complete
-- has no acquisition left to generate, so both are skipped.

DO $$
DECLARE
  obs_ids d_observation_id[];
BEGIN
  SELECT array_agg(o.c_observation_id) INTO obs_ids
    FROM t_observation o
    LEFT JOIN t_obscalc c ON c.c_observation_id = o.c_observation_id
   WHERE o.c_observing_mode_type = 'flamingos_2_mos'
     AND o.c_existence = 'present'
     AND COALESCE(c.c_sci_execution_state NOT IN ('completed', 'declared_complete'), true);

  IF obs_ids IS NOT NULL THEN
    CALL invalidate_obscalc_many(obs_ids);
  END IF;
END;
$$;
