-- Updates the action taken when a program's ToO ceiling (set at proposal
-- acceptance) is lowered.  In this case outstanding live ToO triggers at a
-- higher than accepted ToO activation status must be withdrawn. This is
-- accomplished by clearing the `ready` user workflow status, which returns the
-- observation to a DEFINED state.  The ToO status update itself is handled by
-- too_trigger_track_ready() which executes when the user workflow status changes.

CREATE OR REPLACE FUNCTION too_trigger_ceiling_withdraw()
  RETURNS trigger AS $$
BEGIN
  UPDATE t_observation o
     SET c_workflow_user_state = NULL
   WHERE o.c_workflow_user_state = 'ready'
     AND EXISTS (
       SELECT 1
         FROM t_too_trigger t
        WHERE t.c_observation_id  = o.c_observation_id
          AND t.c_program_id      = NEW.c_program_id
          AND t.c_status          = 'requested'
          AND t.c_too_activation  > NEW.c_too_activation
     );

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

COMMENT ON FUNCTION too_trigger_ceiling_withdraw() IS
  'Clears the ready state of observations whose outstanding ToO request exceeds a '
  'newly lowered ceiling. The withdrawal arm of too_trigger_track_ready() then '
  'closes the request out, so the status and the observation stay in step.';