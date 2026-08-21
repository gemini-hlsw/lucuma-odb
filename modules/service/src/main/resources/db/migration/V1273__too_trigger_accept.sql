-- Closes a ToO trigger as 'accepted' when its observation begins executing.
--
-- Part 2 of 2 (V1272 adds the status).
--
-- WHY
--
-- Until now 'requested' was the only status a fulfilled request could hold. A
-- trigger stayed 'requested' while its observation executed, completed, and sat
-- in the archive.
--
-- WHEN ACCEPTANCE HAPPENS
--
-- At the first non-slew execution event, which is exactly where v_generator_params
-- puts the not_started -> ongoing boundary:
--
--   WHEN NOT EXISTS (SELECT 1 FROM t_execution_event v
--                     WHERE v.c_observation_id = o.c_observation_id
--                       AND v.c_event_type != 'slew') THEN 'not_started'
--
-- Deriving acceptance from the same condition means ACCEPTED and ONGOING are the
-- same instant by construction rather than by coincidence.  The slew exclusion is
-- that view's ("just slewing to the target doesn't count as execution") and it
-- carries over for free: the event type says so directly.
--
-------------------------------------------------------------------------------
-- Acceptance.
-------------------------------------------------------------------------------

CREATE FUNCTION too_trigger_accept()
  RETURNS trigger AS $$
BEGIN
  UPDATE t_too_trigger
     SET c_status = 'accepted'
   WHERE c_observation_id = NEW.c_observation_id
     AND c_status = 'requested';

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER too_trigger_accept_trigger
  AFTER INSERT ON t_execution_event
  FOR EACH ROW
  WHEN (NEW.c_event_type <> 'slew'::e_execution_event_type)
  EXECUTE FUNCTION too_trigger_accept();

-------------------------------------------------------------------------------
-- Backfill.
-------------------------------------------------------------------------------

-- Requests whose observations have already begun executing.
UPDATE t_too_trigger t
   SET c_status = 'accepted'
 WHERE t.c_status = 'requested'
   AND EXISTS (
     SELECT 1
       FROM t_execution_event e
      WHERE e.c_observation_id = t.c_observation_id
        AND e.c_event_type <> 'slew'::e_execution_event_type
   );

-------------------------------------------------------------------------------
-- Documentation.
-------------------------------------------------------------------------------

COMMENT ON TYPE e_too_trigger_status IS
  'ToO trigger lifecycle. requested is the only non-terminal status; accepted '
  '(the observatory began executing), declined (an observer said no), withdrawn '
  '(the PI took it back) and superseded (replaced by a request at a different '
  'activation) are all terminal.';

COMMENT ON FUNCTION too_trigger_accept() IS
  'Closes an observation''s outstanding ToO request as accepted at its first '
  'non-slew execution event.';

COMMENT ON TABLE t_too_trigger IS
  'Records each attempt to activate a ToO observation. Maintained by '
  'too_trigger_track_ready() when the observation user workflow state is updated.'
  'Closed out by too_trigger_accept() when execution begins. At most one row per '
  'observation is requested at a time (i_too_trigger_active); accepted, declined, '
  'withdrawn and superseded attempts accumulate as history.';