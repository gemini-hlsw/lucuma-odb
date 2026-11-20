-- Recording a visit abandons the ongoing steps left behind by the previous
-- visit on the same instrument (V1337).  Those steps may belong to another
-- observation, whose obscalc result (its remaining sequence and workflow state)
-- depends on them.  Nothing invalidated it though: visit_invalidate_trigger
-- covers only the new visit's own observation, and step_obscalc_invalidate
-- fires only on transitions into or out of 'completed'.  The other observation
-- stayed 'ready' with a stale result until something else touched it.
--
-- Invalidating it directly from the visit would have the visit transaction
-- hold the t_obscalc rows of several observations at once, which can deadlock
-- with any other transaction that invalidates the same observations in a
-- different order (a bulk observation edit, for example).  Instead the visit
-- records a mark naming each observation whose steps it abandoned, and the
-- obscalc worker turns marks into invalidations, one observation per
-- transaction (ObscalcService.invalidateMarked).  Until then the observation
-- stays 'ready' with its previous result, for at most a poll period.

-- Observations to invalidate, recorded by transactions that must not lock
-- another observation's t_obscalc row.  There may be many marks per
-- observation.  No foreign key: inserting a mark takes no lock on the
-- observation, and invalidating a deleted observation does nothing.
CREATE TABLE t_obscalc_mark (
  c_mark_id        bigserial        NOT NULL PRIMARY KEY,
  c_observation_id d_observation_id NOT NULL
);

CREATE INDEX i_obscalc_mark_observation ON t_obscalc_mark (c_observation_id);

-- Marks the observations, other than the new visit's own, whose steps the
-- visit abandons.  The visit's own observation is invalidated directly by
-- visit_invalidate_trigger.
CREATE OR REPLACE FUNCTION update_execution_information_for_visit()
  RETURNS TRIGGER AS $$
BEGIN

  PERFORM lock_observation_execution(NEW.c_observation_id);

  WITH abandoned AS (
    UPDATE t_step_execution se
       SET c_execution_state = 'abandoned'
      FROM t_visit v
     WHERE v.c_visit_id   = se.c_visit_id
       AND v.c_instrument = NEW.c_instrument
       AND se.c_visit_id <> NEW.c_visit_id
       AND se.c_execution_state = 'ongoing'
    RETURNING se.c_observation_id
  )
  INSERT INTO t_obscalc_mark (c_observation_id)
  SELECT DISTINCT c_observation_id
    FROM abandoned
   WHERE c_observation_id <> NEW.c_observation_id;

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;
