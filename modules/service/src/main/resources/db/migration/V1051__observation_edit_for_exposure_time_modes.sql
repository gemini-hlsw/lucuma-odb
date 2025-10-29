-- Send an observationEdit notification for tables with a c_observation_id but NOT
-- a c_program_id column, by looking up the program ID from t_observation.
-- Inspired by ch_observation_edit_timing_windows in V0952__filter_edit_events.sql
CREATE OR REPLACE FUNCTION ch_observation_edit_associated_table_no_pid_update()
  RETURNS trigger AS $$
DECLARE
  observation record;
  program_id d_program_id;
BEGIN
  observation := COALESCE(NEW, OLD);
  IF ROW(NEW.*) IS DISTINCT FROM ROW(OLD.*) THEN
    program_id := (SELECT c_program_id FROM t_observation WHERE c_observation_id = observation.c_observation_id);
    PERFORM pg_notify('ch_observation_edit', observation.c_observation_id || ',' || program_id  || ',' || 'UPDATE');
  END IF;
  RETURN observation;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER ch_observation_edit_exposure_time_mode_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_exposure_time_mode
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_observation_edit_associated_table_no_pid_update();
