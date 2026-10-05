-- Recording a visit abandons the ongoing steps left behind by the previous
-- visit. Restrict the update to visits at the same site.
CREATE OR REPLACE FUNCTION update_execution_information_for_visit()
  RETURNS TRIGGER AS $$
BEGIN

  PERFORM lock_observation_execution(NEW.c_observation_id);

  UPDATE t_step_execution se
     SET c_execution_state = 'abandoned'
    FROM t_visit v
   WHERE v.c_visit_id = se.c_visit_id
     AND v.c_site     = NEW.c_site
     AND se.c_visit_id <> NEW.c_visit_id
     AND se.c_execution_state = 'ongoing';

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;
