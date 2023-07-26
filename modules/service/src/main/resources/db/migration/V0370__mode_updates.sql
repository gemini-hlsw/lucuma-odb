

-- This adds observation update events for changes to t_gmos_north_long_slit and
-- t_gmos_south_long_slit. Because these tables don't include the program id (required for access
-- control) we have to select them explictly.

CREATE OR REPLACE FUNCTION ch_gmos_long_slit_edit()
  RETURNS trigger AS $$
DECLARE
  pid d_program_id;
BEGIN
  SELECT c_program_id INTO pid FROM t_observation WHERE c_observation_id = NEW.c_observation_id;
  PERFORM pg_notify('ch_observation_edit', NEW.c_observation_id || ',' || pid  || ',' || nextval('s_event_id')::text || ',' || TG_OP);
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- CREATE CONSTRAINT TRIGGER ch_gmos_south_long_slit_edit_trigger
--   AFTER INSERT OR UPDATE ON t_gmos_south_long_slit
--   DEFERRABLE
--   FOR EACH ROW
--   EXECUTE PROCEDURE ch_gmos_long_slit_edit();

-- CREATE CONSTRAINT TRIGGER ch_gmos_north_long_slit_edit_trigger
--   AFTER INSERT OR UPDATE ON t_gmos_north_long_slit
--   DEFERRABLE
--   FOR EACH ROW
--   EXECUTE PROCEDURE ch_gmos_long_slit_edit();

