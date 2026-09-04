-- A CHECK constraint may only look at the row it is checking, but two of them
-- call is_telluric_calibration, which reads t_observation:
--
--   t_flamingos_2_long_slit.flamingos_2_long_slit_telluric_science_mode_role_check
--   t_telluric_resolution.check_observation_is_telluric
--
-- That makes each unenforceable in one direction and, worse, breaks
-- dump/restore.  CHECK constraints are created in a dump's pre-data section and
-- pg_dump loads table data in alphabetical order by table name, so a table
-- sorting before t_observation is copied while t_observation is still empty:
-- every telluric row then fails its check and, because a COPY is a single
-- statement, the whole table is rolled back.  psql reports the error and carries
-- on, so the table simply arrives empty.
--
-- t_flamingos_2_long_slit ('f') hits this; t_telluric_resolution ('t_t') is
-- spared only because it happens to sort after t_observation.  Both are fixed
-- here, since that distinction is an accident of naming.
--
-- Each invariant is expressed instead as a deferrable constraint trigger,
-- matching what register_observing_mode already does for the cross-table
-- observing mode consistency check. Triggers are restored post-data, so they
-- never fire during a data load, and deferring to commit means the check itself
-- is insensitive to statement ordering within a transaction (subject to other constraints such as FKs).

-- Flamingos 2 long slit: only a telluric may name the mode it calibrates.

ALTER TABLE t_flamingos_2_long_slit
  DROP CONSTRAINT flamingos_2_long_slit_telluric_science_mode_role_check;

CREATE FUNCTION check_flamingos_2_telluric_science_mode_role()
RETURNS TRIGGER AS $$
BEGIN

  IF NEW.c_telluric_science_mode IS NOT NULL AND NOT is_telluric_calibration(NEW.c_observation_id) THEN
    RAISE EXCEPTION
      'Observation % has c_telluric_science_mode "%" but is not a telluric calibration',
      NEW.c_observation_id,
      NEW.c_telluric_science_mode;
  END IF;

  RETURN NEW;

END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER trigger_t_flamingos_2_long_slit_telluric_science_mode_role
  AFTER INSERT OR UPDATE ON t_flamingos_2_long_slit
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION check_flamingos_2_telluric_science_mode_role();

-- Telluric resolution: only a telluric has one.

ALTER TABLE t_telluric_resolution
  DROP CONSTRAINT check_observation_is_telluric;

CREATE FUNCTION check_telluric_resolution_observation()
RETURNS TRIGGER AS $$
BEGIN

  IF NOT is_telluric_calibration(NEW.c_observation_id) THEN
    RAISE EXCEPTION
      'Observation % has a telluric resolution but is not a telluric calibration',
      NEW.c_observation_id;
  END IF;

  RETURN NEW;

END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER trigger_t_telluric_resolution_observation_is_telluric
  AFTER INSERT OR UPDATE ON t_telluric_resolution
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION check_telluric_resolution_observation();
