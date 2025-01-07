-- A trigger function which sends a 'ch_observation_edit' notify. Can be used
-- for any table associated with an observation that has c_program_id and
-- c_observation_id columns.
CREATE OR REPLACE FUNCTION ch_observation_edit_associated_table_update()
  RETURNS trigger AS $$
DECLARE
  rec record;
BEGIN
  rec := COALESCE(NEW, OLD);
  PERFORM pg_notify('ch_observation_edit', rec.c_observation_id || ',' || rec.c_program_id  || ',' || 'UPDATE');
  RETURN rec;
END;
$$ LANGUAGE plpgsql;

-- Use the function for updates on ITC results and execution digest.

CREATE CONSTRAINT TRIGGER ch_observation_edit_itc_result_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_itc_result
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_observation_edit_associated_table_update();

CREATE CONSTRAINT TRIGGER ch_observation_edit_execution_digest_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_execution_digest
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_observation_edit_associated_table_update();