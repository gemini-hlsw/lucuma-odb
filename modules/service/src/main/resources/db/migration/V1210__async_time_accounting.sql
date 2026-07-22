-- Time accounting moves off the execution-event hot path and into the obscalc
-- worker, which recomputes it asynchronously.

-- 1. Stop invalidating obscalc on t_visit UPDATE.  obscalc reads nothing from
--    t_visit, so the UPDATE branch never produced a meaningful recalculation --
--    it only fired because the old synchronous time-accounting update wrote
--    t_visit on every event.  Now that the worker writes the time-accounting
--    columns back to t_visit (both the result columns and the markers below),
--    keeping the UPDATE branch would create a feedback loop.  The INSERT branch
--    is still wanted (refresh obscalc when a visit is recorded, see V1085) as is
--    DELETE.
DROP TRIGGER visit_invalidate_trigger ON t_visit;

CREATE TRIGGER visit_invalidate_trigger
  AFTER INSERT OR DELETE ON t_visit
  FOR EACH ROW EXECUTE FUNCTION obsid_obscalc_invalidate();

-- 2. Time-accounting-specific invalidation tracking, PER VISIT.  These are
--    bumped ONLY on the specific visit that received an execution event or a
--    dataset QA-state change -- the only two things that alter a visit's charge.
--    The worker recomputes a visit only when c_ta_invalidation > c_ta_update, so:
--      * obscalc invalidations unrelated to time accounting (target/mode edits,
--        a migration that blanket-sets observations to 'pending') recompute
--        nothing; and
--      * an event or QA change in one visit never reprices any OTHER visit of
--        the observation -- in particular it cannot silently rewrite a closed,
--        already-corrected visit with a newly-deployed algorithm.
--    Existing visits get equal timestamps (not dirty), preserving their stored
--    history until a genuine change to that visit occurs.
ALTER TABLE t_visit
  ADD COLUMN c_ta_invalidation timestamp NOT NULL DEFAULT now(),
  ADD COLUMN c_ta_update       timestamp NOT NULL DEFAULT now();

-- Mark a single visit's time accounting dirty and invalidate obscalc so the
-- worker picks the observation up (and the digest/workflow refresh too).
-- Updating t_visit here does not re-fire visit_invalidate_trigger (INSERT/DELETE
-- only), so there is no feedback loop.
CREATE PROCEDURE invalidate_visit_time_accounting(
  visit_id       d_visit_id,
  observation_id d_observation_id
) LANGUAGE plpgsql AS $$
BEGIN
  UPDATE t_visit SET c_ta_invalidation = now() WHERE c_visit_id = visit_id;
  CALL invalidate_obscalc(observation_id);
END;
$$;

-- 3. Recording an execution event invalidates that event's visit's time
--    accounting.  This replaces the old synchronous per-event time-accounting
--    update (removed from ExecutionEventService).  AFTER INSERT fires only for
--    genuinely inserted rows, so idempotency-key replays (INSERT ... ON CONFLICT
--    DO UPDATE) do not re-trigger it.
CREATE FUNCTION event_time_accounting_invalidate()
  RETURNS trigger AS $$
BEGIN
  CALL invalidate_visit_time_accounting(NEW.c_visit_id, NEW.c_observation_id);
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER event_time_accounting_invalidate_trigger
  AFTER INSERT ON t_execution_event
  FOR EACH ROW EXECUTE FUNCTION event_time_accounting_invalidate();

-- 4. A dataset QA-state change (Pass/null <-> Fail/Usable) adds or removes a QA
--    discount, changing the charge for the dataset's -- possibly already closed
--    -- visit.  Invalidate that visit's time accounting (which also invalidates
--    obscalc, as this trigger already did).
CREATE OR REPLACE FUNCTION dataset_qa_state_invalidate()
  RETURNS TRIGGER AS $$
BEGIN
  IF (OLD.c_qa_state IS DISTINCT FROM NEW.c_qa_state) AND (
    ((OLD.c_qa_state IS NULL OR OLD.c_qa_state = 'Pass') AND (NEW.c_qa_state IN ('Fail', 'Usable')))
    OR
    ((OLD.c_qa_state IN ('Fail', 'Usable')) AND (NEW.c_qa_state IS NULL OR NEW.c_qa_state = 'Pass'))
  ) THEN
    CALL invalidate_visit_time_accounting(NEW.c_visit_id, NEW.c_observation_id);
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
