-- GNIRS setups, the same columns as t_gnirs_dynamic so the dynamic config
-- codec and mappings apply unchanged.
CREATE TABLE t_gnirs_daytime_calibration_setup (

  c_set_id         int8             PRIMARY KEY,
  c_observation_id d_observation_id NOT NULL,
  c_instrument     d_tag            NOT NULL DEFAULT ('Gnirs') CHECK (c_instrument = 'Gnirs'),

  -- The observation id feeds obscalc invalidation; the composite FK keeps it the set's.
  FOREIGN KEY (c_set_id, c_observation_id)
    REFERENCES t_daytime_calibration_set(c_set_id, c_observation_id) ON DELETE CASCADE,

  c_exposure_time      interval          NOT NULL,
  c_coadds             int4              NOT NULL,
  c_filter             d_tag             NOT NULL REFERENCES t_gnirs_filter(c_tag),
  c_decker             e_gnirs_decker    NOT NULL,

  -- FPU: exactly one of the slit, the IFU or the non-spectroscopy "other".
  c_fpu_slit           d_tag             REFERENCES t_gnirs_fpu_slit(c_tag),
  c_fpu_other          e_gnirs_fpu_other,
  c_fpu_ifu            e_gnirs_fpu_ifu,
  CONSTRAINT gnirs_daytime_calibration_setup_fpu_check CHECK (
    (c_fpu_slit IS NOT NULL AND c_fpu_other IS     NULL AND c_fpu_ifu IS     NULL) OR
    (c_fpu_slit IS     NULL AND c_fpu_other IS NOT NULL AND c_fpu_ifu IS     NULL) OR
    (c_fpu_slit IS     NULL AND c_fpu_other IS     NULL AND c_fpu_ifu IS NOT NULL)
  ),

  -- Spectroscopy config carried when the acquisition mirror is out of the
  -- beam.  All three are null together when the mirror is in.
  c_prism              d_tag             REFERENCES t_gnirs_prism(c_tag),
  c_grating            d_tag             REFERENCES t_gnirs_grating(c_tag),
  c_grating_wavelength d_wavelength_pm,
  CONSTRAINT gnirs_daytime_calibration_setup_acquisition_mirror_check CHECK (
    (c_prism IS     NULL AND c_grating IS     NULL AND c_grating_wavelength IS     NULL) OR
    (c_prism IS NOT NULL AND c_grating IS NOT NULL AND c_grating_wavelength IS NOT NULL)
  ),

  c_camera             d_tag             NOT NULL REFERENCES t_gnirs_camera(c_tag),

  -- Null means "Best" (instrument-chosen) focus.
  c_focus_motor_steps  int4,

  c_read_mode          d_tag             NOT NULL
);

-- The same computed columns as v_gnirs_dynamic, keyed on the set.
CREATE VIEW v_gnirs_daytime_calibration_setup AS
  SELECT t.*,
  CASE WHEN t.c_prism IS NOT NULL THEN t.c_set_id END AS c_acquisition_mirror_out_id,
  COALESCE(
    t.c_grating_wavelength,
    (SELECT f.c_wavelength FROM t_gnirs_filter f WHERE f.c_tag = t.c_filter),
    1650000::d_wavelength_pm
  ) AS c_central_wavelength
FROM
  t_gnirs_daytime_calibration_setup t;

-- A set with a GNIRS setup is anything but a bias.  The kind lives on the set
-- row, so this is a cross-table check and goes in a constraint trigger, fired
-- from either side.  The list mirrors InstrumentSetup.allowedKinds.
CREATE OR REPLACE FUNCTION check_gnirs_daytime_calibration_set_kind()
  RETURNS trigger AS $$
DECLARE
  kind e_calibration_set_type;
BEGIN
  SELECT s.c_kind INTO kind
    FROM t_daytime_calibration_set s
    JOIN t_gnirs_daytime_calibration_setup g ON g.c_set_id = s.c_set_id
   WHERE s.c_set_id = NEW.c_set_id;
  IF FOUND AND kind NOT IN ('dark', 'flat', 'arc', 'daytime_observe', 'smart_flat', 'smart_arc', 'day_baseline') THEN
    RAISE EXCEPTION 'A GNIRS calibration set must be a DARK, FLAT, ARC, DAYTIME_OBSERVE, SMART_FLAT, SMART_ARC or DAY_BASELINE, not %', kind;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER trigger_t_gnirs_daytime_calibration_setup_kind
  AFTER INSERT OR UPDATE ON t_gnirs_daytime_calibration_setup
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION check_gnirs_daytime_calibration_set_kind();

CREATE CONSTRAINT TRIGGER trigger_t_daytime_calibration_set_gnirs_kind
  AFTER UPDATE OF c_kind ON t_daytime_calibration_set
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION check_gnirs_daytime_calibration_set_kind();

CREATE TRIGGER gnirs_daytime_calibration_setup_invalidate_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_gnirs_daytime_calibration_setup
  FOR EACH ROW EXECUTE FUNCTION obsid_obscalc_invalidate();

CREATE TRIGGER gnirs_daytime_calibration_setup_edit_notify_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_gnirs_daytime_calibration_setup
  FOR EACH ROW EXECUTE FUNCTION ch_observation_edit_by_observation();
