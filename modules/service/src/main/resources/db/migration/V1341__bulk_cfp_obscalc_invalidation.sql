-- Editing a CfP's instrument list could take longer than the request timeout
-- (sc-10718).  The t_gemini_cfp_instrument trigger from V1001 ran once per
-- row, and each run invalidated every observation of every program on the CfP
-- one observation at a time via invalidate_obscalc.  A CfP with ~4,500
-- observations and 9 instruments meant ~40,000 procedure calls in a single
-- transaction.  The same trigger also read NEW on DELETE, where it is NULL, so
-- removing an instrument invalidated nothing.
--
-- Here the invalidation becomes set-based and the instrument trigger fires
-- once per statement.

-- Set-based counterpart of invalidate_obscalc, with the same semantics: rows
-- are created if missing, observations that no longer exist are ignored, and an
-- observation being calculated keeps its state but has its invalidation time
-- bumped.
CREATE OR REPLACE PROCEDURE invalidate_obscalc_many(
  observation_ids d_observation_id[]
)
  LANGUAGE plpgsql AS $$
BEGIN
  INSERT INTO t_obscalc (c_program_id, c_observation_id)
  SELECT c_program_id, c_observation_id
    FROM t_observation
   WHERE c_observation_id = ANY(observation_ids)
  ON CONFLICT ON CONSTRAINT t_obscalc_pkey DO NOTHING;

  -- Lock in a consistent order so that concurrent bulk invalidations cannot
  -- deadlock one another.
  PERFORM 1
     FROM t_obscalc
    WHERE c_observation_id = ANY(observation_ids)
    ORDER BY c_observation_id
      FOR UPDATE;

  UPDATE t_obscalc
     SET c_last_invalidation = now(),
         c_failure_count     = 0,
         c_retry_at          = NULL,
         c_obscalc_state     = CASE
                                 WHEN c_obscalc_state = 'calculating' :: e_calculation_state
                                   THEN c_obscalc_state
                                 ELSE 'pending' :: e_calculation_state
                               END
   WHERE c_observation_id = ANY(observation_ids);
END;
$$;

-- Invalidate the obscalc results for all observations in the program
CREATE OR REPLACE PROCEDURE invalidate_all_obscalc_for_program(
  pid d_program_id
)
  LANGUAGE plpgsql AS $$
DECLARE
  obs_ids d_observation_id[];
BEGIN
  SELECT array_agg(c_observation_id) INTO obs_ids
    FROM t_observation
   WHERE c_program_id = pid
     AND c_existence  = 'present';

  CALL invalidate_obscalc_many(obs_ids);
END;
$$;

-- Invalidate the obscalc results for all observations in programs that are
-- assigned any of the given CfPs
CREATE OR REPLACE PROCEDURE invalidate_all_obscalc_for_cfps(
  cfp_ids d_cfp_id[]
)
  LANGUAGE plpgsql AS $$
DECLARE
  obs_ids d_observation_id[];
BEGIN
  SELECT array_agg(o.c_observation_id) INTO obs_ids
    FROM t_proposal p
    JOIN t_observation o ON o.c_program_id = p.c_program_id
   WHERE p.c_cfp_id    = ANY(cfp_ids)
     AND o.c_existence = 'present';

  CALL invalidate_obscalc_many(obs_ids);
END;
$$;

CREATE OR REPLACE PROCEDURE invalidate_all_obscalc_for_cfp(
  cfp_id d_cfp_id
)
  LANGUAGE plpgsql AS $$
BEGIN
  CALL invalidate_all_obscalc_for_cfps(ARRAY[cfp_id]);
END;
$$;

-- Statement-level replacement for the per-row instrument trigger.  Transition
-- tables cannot be declared on a trigger with more than one event, so there is
-- one trigger per event, all sharing this function.
CREATE OR REPLACE FUNCTION cfp_instrument_obscalc_invalidate()
  RETURNS TRIGGER AS $$
DECLARE
  cfp_ids d_cfp_id[];
BEGIN
  IF TG_OP = 'INSERT' THEN
    SELECT array_agg(DISTINCT c_cfp_id) INTO cfp_ids FROM new_rows;
  ELSIF TG_OP = 'DELETE' THEN
    SELECT array_agg(DISTINCT c_cfp_id) INTO cfp_ids FROM old_rows;
  ELSE
    SELECT array_agg(DISTINCT c_cfp_id) INTO cfp_ids
      FROM (SELECT c_cfp_id FROM old_rows UNION SELECT c_cfp_id FROM new_rows) r;
  END IF;

  IF cfp_ids IS NOT NULL THEN
    CALL invalidate_all_obscalc_for_cfps(cfp_ids);
  END IF;

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER cfp_instrument_invalidate_obscalc_trigger ON t_gemini_cfp_instrument;

CREATE TRIGGER cfp_instrument_insert_invalidate_obscalc_trigger
  AFTER INSERT ON t_gemini_cfp_instrument
  REFERENCING NEW TABLE AS new_rows
  FOR EACH STATEMENT
  EXECUTE FUNCTION cfp_instrument_obscalc_invalidate();

CREATE TRIGGER cfp_instrument_delete_invalidate_obscalc_trigger
  AFTER DELETE ON t_gemini_cfp_instrument
  REFERENCING OLD TABLE AS old_rows
  FOR EACH STATEMENT
  EXECUTE FUNCTION cfp_instrument_obscalc_invalidate();

CREATE TRIGGER cfp_instrument_update_invalidate_obscalc_trigger
  AFTER UPDATE ON t_gemini_cfp_instrument
  REFERENCING OLD TABLE AS old_rows NEW TABLE AS new_rows
  FOR EACH STATEMENT
  EXECUTE FUNCTION cfp_instrument_obscalc_invalidate();
