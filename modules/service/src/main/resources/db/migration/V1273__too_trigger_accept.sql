-- Closes a ToO trigger as 'accepted' when its observation begins executing.
--
-- Part 2 of 2 (V1272 adds the status).
--
-- WHY
--
-- Until now 'requested' was the only status a fulfilled request could hold: a
-- trigger stayed 'requested' while its observation executed, completed, and sat in
-- the archive.  That leaves the record open to being rewritten long after the
-- fact.  too_trigger_ceiling_withdraw() (V1268) sweeps 'requested' rows
-- program-wide when the TAC lowers the ceiling, and nothing about the observation
-- gates it -- the TAC edits the proposal, not the observation -- so a ToO that was
-- observed last semester would be recorded as withdrawn by its PI.  Closing the
-- request out when it is answered puts it beyond that sweep.
--
-- It also makes 'requested' mean what readers assume it means.  An observer
-- dashboard filtering the tooTriggers query, or the ch_too_trigger_edit channel,
-- on REQUESTED wants outstanding requests; before this it also got every executing
-- and completed ToO, with no server-side way to tell them apart.  And the
-- transition it most wants to hear about -- somebody picked this up -- now fires on
-- that channel instead of being invisible.
--
-- WHAT IT DOES NOT DO
--
-- It does not touch t_observation.  An earlier draft cleared the observation's
-- 'ready' state here, to keep "there is a live request" and "the observation is
-- asking" the same fact.  That bought little: every path that could rewrite a
-- spent request keys on c_status = 'requested' and so is already excluded by the
-- new status, apart from the successor insert in too_trigger_track_ready(), which
-- V1275 guards directly and far more cheaply.  It cost a good deal: a write to
-- t_observation from inside an execution-event trigger, in the opposite order to
-- every other writer of that pair, which deadlocks against a concurrent
-- observation edit unless the observation row is locked first.
--
-- So 'ready' survives acceptance, and means what it says: the PI asked for this.
-- Whether the ask was answered is the trigger's business, and the two are allowed
-- to differ once it has been.
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
-- An earlier draft fired on the first observe visit instead.  That is the moment
-- V1206 freezes the original time estimate, but it is a little earlier than
-- execution: a visit can be recorded and then abandoned before any step, which
-- would spend the request on an observation that never ran.
--
-- One case still parts the two: c_declared_state outranks the event test in that
-- view, so staff declaring an observation ongoing in visitor mode produces ONGOING
-- with no events and so no acceptance.
--
-- WHAT THIS DELIBERATELY DOES NOT ADD
--
-- Any way to deny a trigger after acceptance.  A failed acquisition, or any other
-- reason execution does not finish, is a fact about the observation, not about the
-- request: the observatory did act, which is the only question a trigger asks.
-- Recording it here would blunt 'declined', whose value is precisely that it means
-- somebody looked and passed it over WITHOUT observing.  The failure is already
-- recorded, and recorded better, by the visit, its steps' execution states
-- (aborted / stopped / abandoned, V0875) and their events.
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

-- No is-this-a-ToO test is needed: the UPDATE matches nothing for anything else.
-- The WHEN clause keeps slew events out without entering the function at all, and
-- t_execution_event is indexed on c_observation_id (V1100), so what an ordinary
-- event pays is one index miss.
CREATE TRIGGER too_trigger_accept_trigger
  AFTER INSERT ON t_execution_event
  FOR EACH ROW
  WHEN (NEW.c_event_type <> 'slew'::e_execution_event_type)
  EXECUTE FUNCTION too_trigger_accept();

-- The existing machinery on t_too_trigger needs no change and picks the
-- transition up for free: chron_too_trigger_update() audits it (so requestedAt ->
-- the accepting chron row is the response latency, available without walking the
-- events again), and ch_too_trigger_edit() broadcasts it, so a dashboard
-- filtering the channel on status learns that a request was taken up instead of
-- having to poll the observation.  i_too_trigger_active likewise needs no change:
-- it indexes 'requested' alone, and an accepted row simply leaves it.

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

-- This fires the ordinary chronicle and notification triggers, so the backfill
-- lands in the audit trail as a migration-time transition (with a null c_user, no
-- session having set lucuma.user) and emits one ch_too_trigger_edit per row.  The
-- affected set is ToO observations that have been observed, which is small, and no
-- subscriber is listening mid-migration.

-------------------------------------------------------------------------------
-- Documentation.
-------------------------------------------------------------------------------

COMMENT ON TYPE e_too_trigger_status IS
  'ToO trigger lifecycle. requested is the only non-terminal status; accepted '
  '(the observatory began executing), declined (an observer said no), withdrawn '
  '(the PI took it back) and superseded (replaced by a request at a different '
  'activation) are all terminal. The last three are equivalent to never having '
  'been triggered. accepted is not: the request was answered, so a further attempt '
  'at the observation is a new request rather than a reopening of this one.';

COMMENT ON FUNCTION too_trigger_accept() IS
  'Closes an observation''s outstanding ToO request as accepted at its first '
  'non-slew execution event, which is where v_generator_params puts the '
  'not_started -> ongoing boundary. Deliberately does not touch t_observation: '
  'the ready state means the PI asked, and survives the ask being answered.';

COMMENT ON TABLE t_too_trigger IS
  'One row per attempt to activate a ToO observation, maintained by '
  'too_trigger_track_ready() from the observation user state and its derived '
  'activation, and closed out by too_trigger_accept() when execution begins. At '
  'most one row per observation is requested at a time (i_too_trigger_active); '
  'accepted, declined, withdrawn and superseded attempts accumulate as history.';
