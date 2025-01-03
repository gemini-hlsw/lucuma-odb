-- Add the program id to the execution event table to make it easier to notify
ALTER TABLE t_execution_event
  ADD COLUMN c_program_id d_program_id;

-- Fill in the program id for existing entries using the observation id.
UPDATE t_execution_event AS e
   SET c_program_id = o.c_program_id
  FROM t_observation o
 WHERE o.c_observation_id = e.c_observation_id;

-- Make the program id non-nullable
ALTER TABLE t_execution_event
  ALTER COLUMN c_program_id SET NOT NULL;

-- Trigger when the execution event rows are added
CREATE FUNCTION ch_execution_event_added()
  RETURNS trigger AS $$
DECLARE
BEGIN
  PERFORM pg_notify(
    'ch_execution_event_added',
    array_to_string(
      ARRAY[
        NEW.c_execution_event_id,
        NEW.c_program_id,
        NEW.c_observation_id,
        NEW.c_visit_id,
        NEW.c_event_type::text
      ],
      ','
    )
  );
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER ch_execution_event_added_trigger
  AFTER INSERT ON t_execution_event
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW
  EXECUTE PROCEDURE ch_execution_event_added();