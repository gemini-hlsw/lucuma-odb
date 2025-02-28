
-- Trigger when datasets are added or edited
CREATE FUNCTION ch_dataset_edit()
  RETURNS trigger AS $$
DECLARE
  pid d_program_id;
BEGIN

  -- Select the corresponding program id.
  SELECT c_program_id INTO pid
  FROM t_observation
  WHERE c_observation_id = NEW.c_observation_id;

  PERFORM pg_notify(
    'ch_dataset_edit',
    array_to_string(
      ARRAY[
        NEW.c_dataset_id,
        NEW.c_observation_id,
        pid,
        -- isWritten
        CASE WHEN NEW.c_end_time IS NOT NULL THEN 'true' ELSE 'false' END,
        TG_OP
      ],
      ','
    )
  );
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER ch_dataset_edit_trigger
  AFTER INSERT OR UPDATE ON t_dataset
  FOR EACH ROW
  EXECUTE PROCEDURE ch_dataset_edit();