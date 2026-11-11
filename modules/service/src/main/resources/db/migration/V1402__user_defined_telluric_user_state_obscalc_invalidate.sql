-- A telluric inherits its science observation's user state
-- (ObservationValidationInfo.AssociatedScienceObservation), but its workflow is
-- cached in t_obscalc. A generated telluric is recalculated because the
-- calibrations daemon rewrites its configuration whenever the science
-- observation's obscalc settles. A user-defined telluric is never resynced, so
-- nothing recalculated it and it stayed Defined after its science observation
-- became Ready.
CREATE FUNCTION user_defined_telluric_user_state_obscalc_invalidate()
  RETURNS TRIGGER AS $$
DECLARE
  obs_ids d_observation_id[];
BEGIN
  IF NEW.c_group_id IS NOT NULL AND NEW.c_calibration_role IS NULL THEN
    SELECT array_agg(c_observation_id) INTO obs_ids
      FROM t_observation
     WHERE c_group_id        = NEW.c_group_id
       AND c_observation_id <> NEW.c_observation_id
       AND c_existence       = 'present'
       AND c_is_user_defined_telluric;

    IF obs_ids IS NOT NULL THEN
      CALL invalidate_obscalc_many(obs_ids);
    END IF;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER user_defined_telluric_user_state_obscalc_invalidate_trigger
  AFTER UPDATE OF c_workflow_user_state ON t_observation
  FOR EACH ROW
  WHEN (OLD.c_workflow_user_state IS DISTINCT FROM NEW.c_workflow_user_state)
  EXECUTE FUNCTION user_defined_telluric_user_state_obscalc_invalidate();

-- Refresh the user-defined tellurics left stale so far.
DO $$
DECLARE
  obs_ids d_observation_id[];
BEGIN
  SELECT array_agg(c_observation_id) INTO obs_ids
    FROM t_observation
   WHERE c_is_user_defined_telluric
     AND c_existence = 'present';

  IF obs_ids IS NOT NULL THEN
    CALL invalidate_obscalc_many(obs_ids);
  END IF;
END;
$$;
