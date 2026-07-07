-- GHOST IFU observing-mode configuration (t_ghost_ifu) was never wired into the
-- obscalc invalidation or observation-edit notify trigger sets.
-- As a result, editing GHOST detector binning / read mode for example
-- did NOT invalidate the cached ExecutionDigest in t_obscalc.

-- Invalidate the obscalc cache whenever a GHOST IFU configuration row changes
CREATE TRIGGER ghost_ifu_invalidate_trigger
AFTER INSERT OR UPDATE OR DELETE ON t_ghost_ifu
FOR EACH ROW
EXECUTE FUNCTION obsid_obscalc_invalidate();

-- Emit a 'ch_observation_edit' notify so clients can refresh on GHOST IFU
-- configuration edits.
CREATE TRIGGER ch_observation_edit_ghost_ifu_trigger
AFTER INSERT OR UPDATE OR DELETE ON t_ghost_ifu
FOR EACH ROW
EXECUTE FUNCTION ch_observation_edit_associated_table_update();

-- Propagate c_mode_key to t_observation so GHOST observations group correctly
-- under observingModeGroup.  
CREATE TRIGGER observing_mode_key_trigger
AFTER INSERT OR DELETE OR UPDATE OF c_mode_key ON t_ghost_ifu
FOR EACH ROW
EXECUTE FUNCTION trigger_set_observation_mode_key();

-- Initialize mode_key for existing GHOST observations.
UPDATE t_observation AS o
SET c_mode_key = g.c_mode_key
FROM t_ghost_ifu g
WHERE o.c_observation_id = g.c_observation_id;
