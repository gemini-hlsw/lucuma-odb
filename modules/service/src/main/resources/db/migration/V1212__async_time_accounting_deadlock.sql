-- Fix a lock-ordering deadlock introduced by V1210's execution-event trigger.
--
-- Recording an execution event locks t_obscalc (via invalidate_obscalc) and
-- t_visit through invalidate_visit_time_accounting, while the sibling trigger
-- update_execution_information_for_step_event locks t_observation FOR UPDATE.
-- The order these were acquired in depended on the (name-based) firing order of
-- the two triggers on t_execution_event, and it came out REVERSED relative to
-- every other path that touches both rows -- observation edits, recording a
-- (different) visit of the observation, sequence edits -- all of which lock
-- t_observation before t_obscalc.  So an event insert could deadlock with any
-- such concurrent transaction on the same observation.  (Two event inserts do
-- NOT deadlock with each other: they serialize on the visit or obscalc row.
-- A production deadlock report showed the event-insert side; the partner is one
-- of the observation-first paths above.)
--
-- Make the lock order a property of THIS procedure rather than of trigger names:
-- acquire the observation lock explicitly, first, so the canonical order
--
--     t_observation  ->  t_obscalc  ->  t_visit
--
-- holds no matter when the trigger fires (and cannot be broken by renaming or
-- adding triggers).
CREATE OR REPLACE PROCEDURE invalidate_visit_time_accounting(
  visit_id       d_visit_id,
  observation_id d_observation_id
) LANGUAGE plpgsql AS $$
BEGIN
  -- Lock the observation first.  This is the same lock
  -- update_execution_information_for_step_event takes, and it precedes the
  -- t_obscalc lock that invalidate_obscalc acquires, so the order can't invert.
  PERFORM 1 FROM t_observation WHERE c_observation_id = observation_id FOR UPDATE;
  CALL invalidate_obscalc(observation_id);
  UPDATE t_visit SET c_ta_invalidation = now() WHERE c_visit_id = visit_id;
END;
$$;
