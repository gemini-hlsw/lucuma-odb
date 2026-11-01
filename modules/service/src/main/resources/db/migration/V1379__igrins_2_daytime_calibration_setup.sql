-- IGRINS-2 setups, the same columns as t_igrins_2_dynamic so the dynamic
-- config codec and mappings apply unchanged.
CREATE TABLE t_igrins_2_daytime_calibration_setup (

  c_set_id         int8             PRIMARY KEY,
  c_observation_id d_observation_id NOT NULL,
  c_instrument     d_tag            NOT NULL DEFAULT ('Igrins2') CHECK (c_instrument = 'Igrins2'),

  -- The observation id feeds obscalc invalidation; the composite FK keeps it the set's.
  FOREIGN KEY (c_set_id, c_observation_id)
    REFERENCES t_daytime_calibration_set(c_set_id, c_observation_id) ON DELETE CASCADE,

  c_exposure_time  interval         NOT NULL
);

CREATE VIEW v_igrins_2_daytime_calibration_setup AS
  SELECT t.*
FROM
  t_igrins_2_daytime_calibration_setup t;

-- A set with an IGRINS-2 setup is a dark, a flat, an arc or a smart kind.  The kind lives
-- on the set row, so this is a cross-table check and goes in a constraint
-- trigger, fired from either side.  The list mirrors InstrumentSetup.allowedKinds.
CREATE OR REPLACE FUNCTION check_igrins_2_daytime_calibration_set_kind()
  RETURNS trigger AS $$
DECLARE
  kind e_calibration_set_type;
BEGIN
  SELECT s.c_kind INTO kind
    FROM t_daytime_calibration_set s
    JOIN t_igrins_2_daytime_calibration_setup i ON i.c_set_id = s.c_set_id
   WHERE s.c_set_id = NEW.c_set_id;
  IF FOUND AND kind NOT IN ('dark', 'flat', 'arc', 'smart_flat', 'smart_arc', 'day_baseline') THEN
    RAISE EXCEPTION 'An IGRINS-2 calibration set must be a DARK, FLAT, ARC, SMART_FLAT, SMART_ARC or DAY_BASELINE, not %', kind;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER trigger_t_igrins_2_daytime_calibration_setup_kind
  AFTER INSERT OR UPDATE ON t_igrins_2_daytime_calibration_setup
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION check_igrins_2_daytime_calibration_set_kind();

CREATE CONSTRAINT TRIGGER trigger_t_daytime_calibration_set_igrins_2_kind
  AFTER UPDATE OF c_kind ON t_daytime_calibration_set
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION check_igrins_2_daytime_calibration_set_kind();

CREATE TRIGGER igrins_2_daytime_calibration_setup_invalidate_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_igrins_2_daytime_calibration_setup
  FOR EACH ROW EXECUTE FUNCTION obsid_obscalc_invalidate();

CREATE TRIGGER igrins_2_daytime_calibration_setup_edit_notify_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_igrins_2_daytime_calibration_setup
  FOR EACH ROW EXECUTE FUNCTION ch_observation_edit_by_observation();
