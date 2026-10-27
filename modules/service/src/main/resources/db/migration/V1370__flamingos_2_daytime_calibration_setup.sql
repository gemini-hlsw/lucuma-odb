-- Flamingos 2 setups, the same columns as t_flamingos_2_dynamic so the
-- dynamic config codec and mappings apply unchanged.
CREATE TABLE t_flamingos_2_daytime_calibration_setup (

  c_set_id         int8             PRIMARY KEY,
  c_observation_id d_observation_id NOT NULL,
  c_instrument     d_tag            NOT NULL DEFAULT ('Flamingos2') CHECK (c_instrument = 'Flamingos2'),

  -- The observation id feeds obscalc invalidation; the composite FK keeps it the set's.
  FOREIGN KEY (c_set_id, c_observation_id)
    REFERENCES t_daytime_calibration_set(c_set_id, c_observation_id) ON DELETE CASCADE,

  c_exposure_time  interval         NOT NULL,

  c_disperser      d_tag                     REFERENCES t_f2_disperser(c_tag),
  c_filter         d_tag            NOT NULL REFERENCES t_f2_filter(c_tag),
  c_read_mode      d_tag            NOT NULL REFERENCES t_f2_read_mode(c_tag),
  c_lyot_wheel     d_tag            NOT NULL REFERENCES t_f2_lyot_wheel(c_tag),

  c_fpu_custom_mask_attachment_id d_attachment_id REFERENCES t_attachment(c_attachment_id),
  c_fpu_custom_mask_slit_width    d_tag           REFERENCES t_f2_custom_slit_width(c_tag),
  c_fpu_builtin                   d_tag           REFERENCES t_f2_fpu(c_tag),
  CHECK (
    (c_fpu_custom_mask_slit_width IS NULL     AND c_fpu_custom_mask_attachment_id IS NULL) OR
    (c_fpu_custom_mask_slit_width IS NOT NULL AND c_fpu_builtin IS NULL)
  ),

  c_decker         d_tag            NOT NULL REFERENCES t_f2_decker(c_tag),
  c_readout_mode   d_tag            NOT NULL REFERENCES t_f2_readout_mode(c_tag),
  c_reads          d_tag            NOT NULL REFERENCES t_f2_reads(c_tag),
  c_window_cover   d_tag                     REFERENCES t_f2_window_cover(c_tag)
);

CREATE VIEW v_flamingos_2_daytime_calibration_setup AS
  SELECT t.*,
  f.c_wavelength AS c_central_wavelength,
  CASE WHEN t.c_fpu_custom_mask_slit_width IS NOT NULL                                THEN t.c_set_id END AS c_fpu_custom_mask_id,
  CASE WHEN t.c_fpu_custom_mask_slit_width IS NOT NULL OR t.c_fpu_builtin IS NOT NULL THEN t.c_set_id END AS c_fpu_id
FROM
  t_flamingos_2_daytime_calibration_setup t
INNER JOIN t_f2_filter f ON t.c_filter = f.c_tag;

-- A set with a Flamingos 2 setup is a dark, a flat or an arc.  The kind lives
-- on the set row, so this is a cross-table check and goes in a constraint
-- trigger, fired from either side.
CREATE OR REPLACE FUNCTION check_flamingos_2_daytime_calibration_set_kind()
  RETURNS trigger AS $$
DECLARE
  kind e_calibration_set_type;
BEGIN
  SELECT s.c_kind INTO kind
    FROM t_daytime_calibration_set s
    JOIN t_flamingos_2_daytime_calibration_setup f ON f.c_set_id = s.c_set_id
   WHERE s.c_set_id = NEW.c_set_id;
  IF FOUND AND kind NOT IN ('dark', 'flat', 'arc') THEN
    RAISE EXCEPTION 'A Flamingos 2 calibration set must be a DARK, FLAT or ARC, not %', kind;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER trigger_t_flamingos_2_daytime_calibration_setup_kind
  AFTER INSERT OR UPDATE ON t_flamingos_2_daytime_calibration_setup
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION check_flamingos_2_daytime_calibration_set_kind();

CREATE CONSTRAINT TRIGGER trigger_t_daytime_calibration_set_flamingos_2_kind
  AFTER UPDATE OF c_kind ON t_daytime_calibration_set
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION check_flamingos_2_daytime_calibration_set_kind();

CREATE TRIGGER flamingos_2_daytime_calibration_setup_invalidate_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_flamingos_2_daytime_calibration_setup
  FOR EACH ROW EXECUTE FUNCTION obsid_obscalc_invalidate();

CREATE TRIGGER flamingos_2_daytime_calibration_setup_edit_notify_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_flamingos_2_daytime_calibration_setup
  FOR EACH ROW EXECUTE FUNCTION ch_observation_edit_by_observation();
