-- GHOST setups, the same columns as t_ghost_dynamic so the dynamic config
-- codec and mappings apply unchanged.
CREATE TABLE t_ghost_daytime_calibration_setup (

  c_set_id         int8             PRIMARY KEY,
  c_observation_id d_observation_id NOT NULL,
  c_instrument     d_tag            NOT NULL DEFAULT ('Ghost') CHECK (c_instrument = 'Ghost'),

  -- The observation id feeds obscalc invalidation; the composite FK keeps it the set's.
  FOREIGN KEY (c_set_id, c_observation_id)
    REFERENCES t_daytime_calibration_set(c_set_id, c_observation_id) ON DELETE CASCADE,

  c_red_exposure_time   interval NOT NULL,
  c_red_exposure_count  int4     NOT NULL,
  c_red_binning         d_tag    NOT NULL REFERENCES t_ghost_binning(c_tag),
  c_red_read_mode       d_tag    NOT NULL REFERENCES t_ghost_read_mode(c_tag),

  c_blue_exposure_time  interval NOT NULL,
  c_blue_exposure_count int4     NOT NULL,
  c_blue_binning        d_tag    NOT NULL REFERENCES t_ghost_binning(c_tag),
  c_blue_read_mode      d_tag    NOT NULL REFERENCES t_ghost_read_mode(c_tag),

  c_ifu1_fiber_agitator e_ghost_fiber_agitator NOT NULL DEFAULT 'disabled',
  c_ifu2_fiber_agitator e_ghost_fiber_agitator NOT NULL DEFAULT 'disabled'
);

-- A set with a GHOST setup is anything but a daytime observe.  The kind lives
-- on the set row, so this is a cross-table check and goes in a constraint
-- trigger, fired from either side.  The list mirrors InstrumentSetup.allowedKinds.
CREATE OR REPLACE FUNCTION check_ghost_daytime_calibration_set_kind()
  RETURNS trigger AS $$
DECLARE
  kind e_calibration_set_type;
BEGIN
  SELECT s.c_kind INTO kind
    FROM t_daytime_calibration_set s
    JOIN t_ghost_daytime_calibration_setup g ON g.c_set_id = s.c_set_id
   WHERE s.c_set_id = NEW.c_set_id;
  IF FOUND AND kind NOT IN ('bias', 'dark', 'flat', 'arc', 'smart_flat', 'smart_arc', 'day_baseline') THEN
    RAISE EXCEPTION 'A GHOST calibration set must be a BIAS, DARK, FLAT, ARC, SMART_FLAT, SMART_ARC or DAY_BASELINE, not %', kind;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER trigger_t_ghost_daytime_calibration_setup_kind
  AFTER INSERT OR UPDATE ON t_ghost_daytime_calibration_setup
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION check_ghost_daytime_calibration_set_kind();

CREATE CONSTRAINT TRIGGER trigger_t_daytime_calibration_set_ghost_kind
  AFTER UPDATE OF c_kind ON t_daytime_calibration_set
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION check_ghost_daytime_calibration_set_kind();

CREATE TRIGGER ghost_daytime_calibration_setup_invalidate_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_ghost_daytime_calibration_setup
  FOR EACH ROW EXECUTE FUNCTION obsid_obscalc_invalidate();

CREATE TRIGGER ghost_daytime_calibration_setup_edit_notify_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_ghost_daytime_calibration_setup
  FOR EACH ROW EXECUTE FUNCTION ch_observation_edit_by_observation();
