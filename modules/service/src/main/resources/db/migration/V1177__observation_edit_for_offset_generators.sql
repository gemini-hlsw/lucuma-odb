-- Fire an observationEdit (ch_observation_edit) notification when an offset
-- generator or one of its enumerated offsets changes.
CREATE CONSTRAINT TRIGGER ch_observation_edit_offset_generator_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_offset_generator
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_observation_edit_associated_table_no_pid_update();

CREATE CONSTRAINT TRIGGER ch_observation_edit_enumerated_offset_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_enumerated_offset
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_observation_edit_associated_table_no_pid_update();

-- F2 imaging mode-config edits should invalidate obscalc
CREATE TRIGGER flamingos_2_imaging_invalidate_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_flamingos_2_imaging
  FOR EACH ROW
  EXECUTE FUNCTION obsid_obscalc_invalidate();
