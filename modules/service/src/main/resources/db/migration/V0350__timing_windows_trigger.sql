-- Notify of timing windows changes in the observation subscription channel.

CREATE OR REPLACE FUNCTION ch_observation_edit_timing_windows()
  RETURNS trigger AS $$
DECLARE
  observation record;
  program_id d_program_id;
BEGIN
  observation := COALESCE(NEW, OLD);
  program_id := (SELECT c_program_id FROM t_observation WHERE c_observation_id = observation.c_observation_id);
  PERFORM pg_notify('ch_observation_edit', observation.c_observation_id || ',' || program_id  || ',' || 'UPDATE');
  RETURN observation;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER ch_observation_edit_timing_windows_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_timing_window
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_observation_edit_timing_windows();