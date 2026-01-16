-- Prevent deletion of observations that have visits or execution events.

CREATE OR REPLACE FUNCTION check_observation_has_no_execution_data()
RETURNS TRIGGER AS $$
DECLARE
  visit_count INTEGER;
  event_count INTEGER;
BEGIN
  -- calibrations can be hard-deleted, in that case check if there are visits or events,
  IF TG_OP = 'DELETE' THEN
    SELECT COUNT(*) INTO visit_count FROM t_visit WHERE c_observation_id = OLD.c_observation_id;
    IF visit_count > 0 THEN
      RAISE EXCEPTION 'Cannot delete observation % because it has % visit(s)',
        OLD.c_observation_id, visit_count USING ERRCODE = 'P0001';
    END IF;

    SELECT COUNT(*) INTO event_count FROM t_execution_event WHERE c_observation_id = OLD.c_observation_id;
    IF event_count > 0 THEN
      RAISE EXCEPTION 'Cannot delete observation % because it has % execution event(s)',
        OLD.c_observation_id, event_count USING ERRCODE = 'P0001';
    END IF;

    RETURN OLD;

  ELSIF TG_OP = 'UPDATE' THEN
    -- in case of soft-deletions, check if there are visits or events.
    IF NEW.c_existence = 'deleted' AND OLD.c_existence = 'present' THEN
      SELECT COUNT(*) INTO visit_count FROM t_visit WHERE c_observation_id = NEW.c_observation_id;
      IF visit_count > 0 THEN
        RAISE EXCEPTION 'Cannot delete observation % because it has % visit(s)',
          NEW.c_observation_id, visit_count USING ERRCODE = 'P0001';
      END IF;

      SELECT COUNT(*) INTO event_count FROM t_execution_event WHERE c_observation_id = NEW.c_observation_id;
      IF event_count > 0 THEN
        RAISE EXCEPTION 'Cannot delete observation % because it has % execution event(s)',
          NEW.c_observation_id, event_count USING ERRCODE = 'P0001';
      END IF;
    END IF;

    RETURN NEW;
  END IF;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER check_observation_deletion_trigger
  BEFORE DELETE ON t_observation
  FOR EACH ROW
  EXECUTE FUNCTION check_observation_has_no_execution_data();

CREATE TRIGGER check_observation_soft_delete_trigger
  BEFORE UPDATE OF c_existence ON t_observation
  FOR EACH ROW
  EXECUTE FUNCTION check_observation_has_no_execution_data();
