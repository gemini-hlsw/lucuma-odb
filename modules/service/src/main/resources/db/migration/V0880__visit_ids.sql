-- Keep up with the next visit id for this observation.  We want to know what
-- the visit id will be ahead of time so that it doesn't change when the visit
-- is ultimately recorded.  When the visit changes, atom and step ids must
-- change as well.
ALTER TABLE t_observation
  ADD COLUMN c_next_visit_id d_visit_id UNIQUE NOT NULL DEFAULT 'v-' || to_hex(nextval('s_visit_id'));

-- Updates the next visit id for this observation and returns the previous
-- value at the same time.
CREATE FUNCTION next_visit_id(
  observation_id d_observation_id
) RETURNS d_visit_id AS $$
DECLARE
  old_visit_id d_visit_id;
BEGIN

  UPDATE t_observation n
     SET c_next_visit_id = 'v-' || to_hex(nextval('s_visit_id'))
    FROM t_observation o
   WHERE n.c_observation_id = o.c_observation_id
     AND n.c_observation_id = observation_id
    RETURNING o.c_next_visit_id INTO old_visit_id;

  RETURN old_visit_id;

END;
$$ LANGUAGE plpgsql;

-- We'll no longer create new ids when rows are inserted but instead we'll
-- take them from the observation table c_next_visit_id column.
ALTER TABLE t_visit
  ALTER COLUMN c_visit_id DROP DEFAULT;