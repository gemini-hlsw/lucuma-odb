-- A rapid or interrupting request that says nothing about its timing now gets
-- one window: INCLUDE, opening at the moment of the request and closing 24
-- hours later.  A standard ToO waits its turn in the queue like any other
-- observation.
--
-- WHY ONLY WHEN THERE ARE NONE
--
-- Timing windows are the PI's own statement of when the observation is worth
-- doing, and a ToO's windows are frequently the whole point (an eclipse, a
-- visibility run).  A default is a stand-in for an answer nobody gave, so it
-- applies only when nobody gave one.
--
-- WHY THE WINDOW IS OWNED RATHER THAN JUST ADDED
--
-- If we only add a default when none is already present, and never remove it
-- then a ToO triggered, withdrawn, and triggered again a month later would
-- still *have* the window its first request left, so the second request would
-- add nothing and inherit a window that closed weeks ago -- urgent and
-- unschedulable at the same time.
--
-- The new c_automatic column is what makes taking it back safe.  Only this
-- migration's trigger ever sets it, so the deletion can never reach a window
-- the PI wrote, and a window the PI edits stops being automatic by
-- construction: an edit replaces the observation's windows wholesale.

-------------------------------------------------------------------------------
-- Ownership.
-------------------------------------------------------------------------------

ALTER TABLE t_timing_window
  ADD COLUMN c_automatic boolean NOT NULL DEFAULT false;

-- At most one automatic window per observation.  The trigger below maintains
-- this on its own -- it adds a window only to an observation that has none --
-- and saying so here is what lets the deletion speak of "the" automatic window.
CREATE UNIQUE INDEX i_timing_window_automatic
  ON t_timing_window (c_observation_id)
  WHERE c_automatic;

-------------------------------------------------------------------------------
-- The default window.
-------------------------------------------------------------------------------

CREATE FUNCTION too_trigger_default_window()
  RETURNS trigger AS $$
DECLARE
  closes constant timestamp := NEW.c_requested_at + INTERVAL '24 hours';
BEGIN
  IF NEW.c_too_activation >= 'rapid'::e_too_activation
     AND NOT EXISTS (
       SELECT 1
         FROM t_timing_window
        WHERE c_observation_id = NEW.c_observation_id
     )
  THEN
    INSERT INTO t_timing_window (
      c_observation_id,
      c_inclusion,
      c_start,
      c_end_at,
      c_automatic
    ) VALUES (
      NEW.c_observation_id,
      'include'::e_timing_window_inclusion,
      NEW.c_requested_at,
      closes,
      true
    );
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Every insert into t_too_trigger is a live request (c_status defaults to
-- 'requested' and nothing inserts a terminal row), including the successor a
-- supersession mints.
CREATE TRIGGER too_trigger_default_window_trigger
  AFTER INSERT ON t_too_trigger
  FOR EACH ROW
  EXECUTE FUNCTION too_trigger_default_window();

-------------------------------------------------------------------------------
-- Taking it back.
-------------------------------------------------------------------------------

CREATE FUNCTION too_trigger_clear_default_window()
  RETURNS trigger AS $$
BEGIN
  DELETE FROM t_timing_window
   WHERE c_observation_id = OLD.c_observation_id
     AND c_automatic;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- A request that ends without being acted on takes its window with it, so the
-- observation is left as the trigger found it.
--
-- Acceptance is the exception, and the only one: the observatory has begun
-- observing, and deleting the window then would not tidy anything up -- it would
-- hand an executing observation an unbounded schedule, which is the opposite of
-- what the window was for.
--
-- Supersession goes through here like any other close-out, and the successor's
-- insert follows it, so a change of activation replaces the window rather than
-- keeping it: the successor is a different request, made at a different time,
-- and its 24 hours run from its own.
CREATE TRIGGER too_trigger_clear_default_window_trigger
  AFTER UPDATE OF c_status ON t_too_trigger
  FOR EACH ROW
  WHEN (OLD.c_status  = 'requested'::e_too_trigger_status
    AND NEW.c_status <> 'requested'::e_too_trigger_status
    AND NEW.c_status <> 'accepted'::e_too_trigger_status)
  EXECUTE FUNCTION too_trigger_clear_default_window();

-------------------------------------------------------------------------------
-- Documentation.
-------------------------------------------------------------------------------

COMMENT ON COLUMN t_timing_window.c_automatic IS
  'True for a window the database added on the observation''s behalf rather than '
  'one the PI stated, which is what makes it safe to remove again. Set only by '
  'too_trigger_default_window(); every other path leaves it false, so a window '
  'that survives an edit or a clone belongs to whoever holds it now.';

COMMENT ON FUNCTION too_trigger_default_window() IS
  'Gives a newly requested rapid or interrupting ToO trigger a 24 hour INCLUDE '
  'window starting at the request, when the observation has no timing windows of '
  'its own.';

COMMENT ON FUNCTION too_trigger_clear_default_window() IS
  'Removes an observation''s automatic timing window when its request is closed '
  'out without being accepted, leaving the observation as the trigger found it.';
