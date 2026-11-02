-- Observation QA state: the QA verdict for an observation as a whole,
-- calculated from its science datasets unless staff override it.

CREATE TYPE e_observation_qa_state AS ENUM ('pass', 'usable', 'fail');
COMMENT ON TYPE e_observation_qa_state IS 'Observation QA states.';

-- The staff override, if any.  It stays in place as datasets are added or
-- their QA states change, until staff clear it.
ALTER TABLE t_observation
  ADD COLUMN c_qa_state_override e_observation_qa_state NULL;

-- The rollup below reads an observation's datasets.
CREATE INDEX i_dataset_observation ON t_dataset (c_observation_id);

-- Observation QA, one row per observation.  Only written science datasets
-- count: acquisition datasets are ignored, and so are datasets that were never
-- written (no end time), such as those of aborted exposures.  Nobody assesses
-- those, so counting them would leave the observation without a verdict for
-- good.  An exposure in progress counts once it has been written.
--
-- The calculated state is
--
--   no science datasets, or any not yet assessed -> null (undefined)
--   all Pass                                     -> pass
--   all Fail                                     -> fail
--   otherwise                                    -> usable
--
-- The counts are computed per observation in a LATERAL subquery so that a
-- query for a few observations reads only their datasets.
CREATE VIEW v_observation_qa AS
SELECT
  o.c_observation_id,
  COALESCE(o.c_qa_state_override, q.c_computed) AS c_state,
  q.c_computed,
  o.c_qa_state_override                         AS c_override,
  d.c_total,
  d.c_pass,
  d.c_usable,
  d.c_fail,
  d.c_not_assessed
FROM t_observation o
CROSS JOIN LATERAL (
  SELECT
    COUNT(*)::int4                                          AS c_total,
    (COUNT(*) FILTER (WHERE x.c_qa_state = 'pass'))::int4   AS c_pass,
    (COUNT(*) FILTER (WHERE x.c_qa_state = 'usable'))::int4 AS c_usable,
    (COUNT(*) FILTER (WHERE x.c_qa_state = 'fail'))::int4   AS c_fail,
    (COUNT(*) FILTER (WHERE x.c_qa_state IS NULL))::int4    AS c_not_assessed
  FROM t_dataset x
  JOIN t_step s ON s.c_step_id = x.c_step_id
  JOIN t_atom a ON a.c_atom_id = s.c_atom_id
  WHERE x.c_observation_id = o.c_observation_id
    AND x.c_end_time IS NOT NULL
    AND a.c_sequence_type  = 'science'
) d
CROSS JOIN LATERAL (
  SELECT
    CASE
      WHEN d.c_total = 0 OR d.c_not_assessed > 0 THEN NULL
      WHEN d.c_pass  = d.c_total                 THEN 'pass'
      WHEN d.c_fail  = d.c_total                 THEN 'fail'
      ELSE                                            'usable'
    END::e_observation_qa_state AS c_computed
) q;

-- The rollup reads datasets, so a change to one that can move it (its QA
-- state, or its being written) must tell observationEdit subscribers.  Postgres
-- delivers one notification per distinct payload per transaction, so a bulk
-- QA edit sends one per observation rather than one per dataset.
CREATE TRIGGER dataset_qa_observation_edit_trigger
  AFTER UPDATE ON t_dataset
  FOR EACH ROW
  WHEN (OLD.c_qa_state IS DISTINCT FROM NEW.c_qa_state
     OR OLD.c_end_time IS DISTINCT FROM NEW.c_end_time)
  EXECUTE FUNCTION ch_observation_edit_by_observation();

-- Nothing obscalc computes depends on the override, so setting it alone should
-- not queue a recalculation.  This replaces obsid_obscalc_invalidate (V0988)
-- for t_observation only, comparing the rows without the override.
CREATE FUNCTION observation_obscalc_invalidate()
  RETURNS TRIGGER AS $$
BEGIN
  IF TG_OP = 'DELETE' THEN
    CALL invalidate_obscalc(OLD.c_observation_id);
  ELSIF TG_OP = 'INSERT' OR
        (to_jsonb(NEW) - 'c_qa_state_override') IS DISTINCT FROM
        (to_jsonb(OLD) - 'c_qa_state_override') THEN
    CALL invalidate_obscalc(NEW.c_observation_id);
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE TRIGGER observation_invalidate_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_observation
  FOR EACH ROW EXECUTE FUNCTION observation_obscalc_invalidate();
