-- A Flamingos 2 MOS acquisition now takes its sky step before the on-target
-- step, and its Fine Adjustments atoms take no sky step (sc-11452). Both are
-- computed in code but change the generated sequence and its time estimate, so
-- every Flamingos 2 MOS obscalc result is stale.
DO $$
DECLARE
  obs_ids d_observation_id[];
BEGIN
  SELECT array_agg(c_observation_id) INTO obs_ids
    FROM t_observation
   WHERE c_observing_mode_type = 'flamingos_2_mos';

  IF obs_ids IS NOT NULL THEN
    CALL invalidate_obscalc_many(obs_ids);
  END IF;
END;
$$;
