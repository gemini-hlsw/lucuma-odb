-- We need to propagate obe mode from igrins2 to t_observation
CREATE TRIGGER observing_mode_key_trigger
AFTER INSERT OR DELETE OR UPDATE OF c_mode_key ON t_igrins_2_long_slit
FOR EACH ROW
EXECUTE FUNCTION trigger_set_observation_mode_key();

-- send an observation update event when the offsets change
CREATE TRIGGER ch_observation_edit_igrins2_offsets_trigger
AFTER UPDATE OF c_spatial_offsets ON t_igrins_2_long_slit
FOR EACH ROW
EXECUTE FUNCTION ch_observation_edit_associated_table_update();

-- Invalidate obscalc on config changes
CREATE TRIGGER igrins_2_long_slit_invalidate_trigger
AFTER INSERT OR UPDATE OR DELETE ON t_igrins_2_long_slit
FOR EACH ROW EXECUTE FUNCTION obsid_obscalc_invalidate();

-- Initialize mode_key for existing igrins2 observations
UPDATE t_observation AS o
SET c_mode_key = i.c_mode_key
FROM t_igrins_2_long_slit i
WHERE o.c_observation_id = i.c_observation_id;

