-- When instrument configurations change we update c_mode_key.
-- We need to add triggers to update the mode_key on t_observation

-- Flamingos 2 Long Slit Observing Mode
CREATE TRIGGER observing_mode_key_trigger
AFTER INSERT OR DELETE OR UPDATE OF c_mode_key ON t_flamingos_2_long_slit
FOR EACH ROW
EXECUTE FUNCTION trigger_set_observation_mode_key();
