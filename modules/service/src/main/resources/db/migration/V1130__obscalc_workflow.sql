-- Notify when the obscalc state is updated.
CREATE OR REPLACE FUNCTION ch_obscalc_update()
  RETURNS trigger AS $$
DECLARE
  obscalc record;
BEGIN
  IF ROW(NEW.*) IS DISTINCT FROM ROW(OLD.*) THEN
    obscalc := COALESCE(NEW, OLD);
    PERFORM pg_notify(
      'ch_obscalc_update',
      array_to_string(ARRAY[
        obscalc.c_observation_id,
        obscalc.c_program_id,
        COALESCE(OLD.c_obscalc_state::text, 'null'),
        COALESCE(NEW.c_obscalc_state::text, 'null'),
        COALESCE(OLD.c_workflow_state::text, 'null'),
        COALESCE(NEW.c_workflow_state::text, 'null'),
        TG_OP
      ], ',')
    );
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;