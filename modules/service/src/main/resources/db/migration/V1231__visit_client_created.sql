-- Client-supplied visit creation times.
--
-- A visit's recorded time is the moment the ODB first created the row -- either
-- from a slew event (application "Navigate") or a recordVisit (application
-- "Observe").  As with execution events, any delay between the visit actually
-- opening at the telescope and the row being written skews the derived observing
-- night (which drives the daylight discount).  We now let the creating client
-- optionally send the real creation time.  The columns mirror the GraphQL API:
--
--   c_recorded_time  -- when the ODB recorded it, always now() (was c_created);
--                       maps to the `recordedTime` field.
--   c_client_time    -- the raw client-supplied creation time, NULL if absent.
--   c_effective_time -- the creation time we associate with the visit: the client
--                       time when supplied, else the recorded time.  Maps to the
--                       `effectiveTime` field.  Generated, so readers (e.g. the
--                       observing-night lookup) see one clean value.
--
-- Existing rows have no client time, so c_effective_time = c_recorded_time.

ALTER TABLE t_visit
  RENAME COLUMN c_created TO c_recorded_time;

ALTER TABLE t_visit
  ADD COLUMN c_client_time timestamp NULL,
  ADD COLUMN c_effective_time timestamp NOT NULL
             GENERATED ALWAYS AS (COALESCE(c_client_time, c_recorded_time)) STORED;

-- Reject an implausible client creation time.  A visit is always created in real
-- time (there is no backfill/replay of visits), so a legitimate client creation
-- time is within ordinary clock skew of the wall clock -- the same 5 minute
-- tolerance the execution-event check uses.  Anything further out is a broken
-- clock that would misplace the observing night.
--
-- Only c_client_time is validated; a NULL keeps today's behavior and can never
-- be rejected.
CREATE FUNCTION check_visit_client_time()
  RETURNS TRIGGER AS $$
BEGIN
  IF NEW.c_client_time IS NOT NULL
     AND (NEW.c_client_time < now() - interval '5 minutes'
          OR NEW.c_client_time > now() + interval '5 minutes') THEN
    -- Custom SQLSTATE (class 'OD' is not used by PostgreSQL) so the service can
    -- match this specific error rather than any raised exception.  Kept in sync
    -- with ClientTimeError.OutOfRangeSqlState in the Scala code.
    RAISE EXCEPTION 'visit creation time % is out of range', NEW.c_client_time
      USING ERRCODE = 'ODB01';
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER check_visit_client_time_trigger
  BEFORE INSERT ON t_visit
  FOR EACH ROW EXECUTE FUNCTION check_visit_client_time();

-- The execution-event time check (V1230) reads the visit's recorded creation time
-- as its lower bound.  That column was just renamed from c_created, so repoint the
-- function; plpgsql resolves column names at run time, so this must be updated in
-- lockstep with the rename.
CREATE OR REPLACE FUNCTION check_execution_event_client_time()
  RETURNS TRIGGER AS $$
DECLARE
  visit_recorded timestamp;
BEGIN
  IF NEW.c_client_time IS NOT NULL THEN
    SELECT c_recorded_time INTO visit_recorded
      FROM t_visit
     WHERE c_visit_id = NEW.c_visit_id;

    IF NEW.c_client_time < visit_recorded - interval '5 minutes'
       OR NEW.c_client_time > now() + interval '5 minutes' THEN
      RAISE EXCEPTION 'execution event time % is out of range for visit %',
        NEW.c_client_time, NEW.c_visit_id
        USING ERRCODE = 'ODB01';
    END IF;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
