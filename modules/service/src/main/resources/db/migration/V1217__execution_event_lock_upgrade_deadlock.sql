-- Fix the remaining execution-event deadlock: a lock UPGRADE on t_observation.
--
-- Inserting a row into t_execution_event enforces its foreign key to
-- t_observation, and to do so PostgreSQL takes a FOR KEY SHARE lock on the
-- referenced t_observation row.  Crucially, that FK lock is taken as part of the
-- INSERT itself -- BEFORE any AFTER-INSERT trigger runs.  The AFTER triggers
-- (event_time_accounting_invalidate -> invalidate_visit_time_accounting, and
-- update_execution_information_for_step_event) then take FOR UPDATE on the SAME
-- t_observation row.  So every event insert performs a lock UPGRADE on
-- t_observation:  FOR KEY SHARE  ->  FOR UPDATE.
--
-- FOR KEY SHARE is self-compatible, so two concurrent event inserts that
-- reference the same observation (e.g. two dataset events, or a step + dataset
-- event during active execution) are BOTH granted KEY SHARE.  Each then blocks
-- trying to upgrade to FOR UPDATE, which conflicts with the other's KEY SHARE:
-- a textbook lock-upgrade deadlock.  This is the Jul 16 production report (two
-- transactions each "waits for ShareLock on transaction ..." on the other) and
-- it disproves V1212's assumption that two event inserts cannot deadlock with
-- each other -- they deadlock on t_observation before ever reaching t_obscalc or
-- t_visit.
--
-- V1212 could not fix this: it moved the FOR UPDATE to be first *among the
-- triggers*, but a trigger can never precede the FK KEY SHARE lock, which the
-- INSERT takes before any trigger fires.
--
-- The fix is to acquire FOR UPDATE on t_observation BEFORE the FK KEY SHARE, in
-- a BEFORE INSERT trigger.  BEFORE ROW triggers run before the tuple is inserted
-- and before the FK is checked, so the transaction already holds the strong
-- FOR UPDATE lock when the FK check -- and every AFTER trigger -- re-locks the
-- same row.  A stronger lock already held is never upgraded, so there is no
-- upgrade and no race: concurrent same-observation event inserts serialize
-- cleanly on FOR UPDATE, and the canonical lock order
--
--     t_observation  ->  t_obscalc  ->  t_visit
--
-- holds from the very first lock the insert takes.
CREATE FUNCTION lock_observation_for_execution_event()
  RETURNS trigger AS $$
BEGIN
  PERFORM 1 FROM t_observation WHERE c_observation_id = NEW.c_observation_id FOR UPDATE;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER lock_observation_for_execution_event_trigger
  BEFORE INSERT ON t_execution_event
  FOR EACH ROW EXECUTE FUNCTION lock_observation_for_execution_event();
