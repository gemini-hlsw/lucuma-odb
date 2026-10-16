-- Daytime calibration observing mode.  One mode for every instrument: the
-- instrument is a parameter of the config, as the exchange partner instrument
-- is for exchange observations.  The FK through c_instrument ties the
-- observation's instrument to the config, which stored atoms and steps need.
CREATE TABLE t_daytime_calibration (

  c_observation_id      d_observation_id      NOT NULL PRIMARY KEY,
  c_instrument          d_tag                 NOT NULL REFERENCES t_instrument(c_tag),
  c_observing_mode_type e_observing_mode_type NOT NULL DEFAULT 'daytime_calibration'
    CHECK (c_observing_mode_type = 'daytime_calibration'),

  -- Instruments a daytime calibration may use.  GHOST needs an
  -- IFU target mapping for its static config, which a daytime calibration
  -- cannot supply.
  CHECK (c_instrument IN ('Flamingos2', 'GmosNorth', 'GmosSouth', 'Gnirs', 'Igrins2')),

  FOREIGN KEY (c_observation_id, c_instrument, c_observing_mode_type)
    REFERENCES t_observation(c_observation_id, c_instrument, c_observing_mode_type)
    ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED
);

CREATE VIEW v_daytime_calibration AS
  SELECT
    c_observation_id,
    c_instrument,
    c_observing_mode_type
  FROM t_daytime_calibration;

-- Registers the mode and adds the consistency and obscalc invalidation triggers.
SELECT register_observing_mode('daytime_calibration', 't_daytime_calibration');

-- The mode and the daytime calibration role always go together.
ALTER TABLE t_observation
  ADD CONSTRAINT t_observation_daytime_calibration_role_check CHECK (
    (c_observing_mode_type IS NOT DISTINCT FROM 'daytime_calibration') =
    (c_calibration_role    IS NOT DISTINCT FROM 'daytime_calibration')
  );

-- Daytime calibrations have no exposure time mode.
CREATE OR REPLACE FUNCTION check_etm_consistent()
RETURNS TRIGGER AS $$
DECLARE
  obs_id   d_observation_id;
  obs_mode e_observing_mode_type;
  acq_count INTEGER;
  sci_count INTEGER;
BEGIN

  obs_id := COALESCE(NEW.c_observation_id, OLD.c_observation_id);

  SELECT c_observing_mode_type INTO obs_mode
    FROM t_observation
   WHERE c_observation_id = obs_id;

  SELECT
    COUNT(*) FILTER (WHERE c_role = 'acquisition'),
    COUNT(*) FILTER (WHERE c_role = 'science')
  INTO acq_count, sci_count
  FROM t_exposure_time_mode
  WHERE c_observation_id = obs_id;

  IF obs_mode IS NULL THEN

    IF acq_count <> 0 OR sci_count <> 0 THEN
      RAISE EXCEPTION 'Observation % with mode % should not have acquisition nor science exposure time modes', obs_id, obs_mode;
    END IF;

  ELSE

    CASE
      WHEN obs_mode IN ('flamingos_2_long_slit', 'gmos_north_long_slit', 'gmos_south_long_slit', 'gmos_north_ifu', 'gmos_south_ifu') THEN
        IF acq_count <> 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have an acquisition exposure time mode', obs_id, obs_mode;
        END IF;

        IF sci_count <> 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have exactly one science exposure time mode', obs_id, obs_mode;
        END IF;

      -- GNIRS spectroscopy keeps its single acquisition ETM, but science ETMs are
      -- now per central wavelength, in both the 'initial' and 'current' row
      -- versions, so their count is not fixed.
      WHEN obs_mode IN ('gnirs_long_slit', 'gnirs_ifu') THEN
        IF acq_count <> 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have an acquisition exposure time mode', obs_id, obs_mode;
        END IF;

        IF sci_count < 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have at least one science exposure time mode', obs_id, obs_mode;
        END IF;

      WHEN obs_mode IN ('flamingos_2_mos', 'gmos_north_mos', 'gmos_south_mos') THEN
        IF acq_count <> 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have an acquisition exposure time mode', obs_id, obs_mode;
        END IF;

        IF sci_count <> 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have exactly one science exposure time mode', obs_id, obs_mode;
        END IF;

        -- MOS acquisition is sized by the observer, not solved from a
        -- signal-to-noise goal.
        IF NOT EXISTS (
          SELECT 1
            FROM t_exposure_time_mode e
           WHERE e.c_observation_id = obs_id
             AND e.c_role = 'acquisition'
             AND e.c_exposure_time_mode = 'time_and_count'
        ) THEN
          RAISE EXCEPTION 'Observation % with mode % must have a Time & Count acquisition exposure time mode', obs_id, obs_mode;
        END IF;

      WHEN obs_mode = 'ghost_ifu' THEN
        IF sci_count <> 2 THEN
          RAISE EXCEPTION 'Observation % with mode % must have two science exposure time modes (red and blue camera)', obs_id, obs_mode;
        END IF;

      WHEN obs_mode = 'igrins_2_long_slit' THEN
        IF sci_count <> 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have exactly one science exposure time mode', obs_id, obs_mode;
        END IF;

      WHEN obs_mode = 'gnirs_imaging' THEN
        IF acq_count <> 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have an acquisition exposure time mode', obs_id, obs_mode;
        END IF;

      WHEN obs_mode IN ('gmos_north_imaging', 'gmos_south_imaging', 'flamingos_2_imaging') THEN
        NULL;

      -- no checks for visitor modes
      WHEN obs_mode IN (
        'alopeke_speckle',
        'alopeke_wide_field',
        'visitor_north',
        'visitor_south',
        'zorro_speckle',
        'zorro_wide_field',
        'maroon_x'
      ) THEN
        NULL;

      WHEN obs_mode IN ('exchange_keck', 'exchange_subaru', 'daytime_calibration') THEN
        IF acq_count <> 0 OR sci_count <> 0 THEN
          RAISE EXCEPTION 'Observation % with mode % should not have acquisition nor science exposure time modes', obs_id, obs_mode;
        END IF;

      ELSE
        RAISE EXCEPTION 'Unknown observing mode % for observation %', obs_mode, obs_id;
    END CASE;
  END IF;

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

-- Invariant: while an observation is a daytime calibration, every stored step
-- is a dayCal with guiding disabled and there is no acquisition sequence.  The
-- services check first for readable errors; these triggers are the backstop.
-- Cross-table, so constraint triggers rather than CHECKs (see V1308).

CREATE FUNCTION is_daytime_calibration(oid d_observation_id)
RETURNS boolean AS $$
  SELECT EXISTS (
    SELECT 1
      FROM t_observation
     WHERE c_observation_id   = oid
       AND c_calibration_role = 'daytime_calibration'
  )
$$ LANGUAGE sql STABLE;

CREATE FUNCTION check_daytime_calibration_step()
RETURNS TRIGGER AS $$
DECLARE
  oid d_observation_id;
BEGIN

  SELECT c_observation_id INTO oid
    FROM t_atom
   WHERE c_atom_id = NEW.c_atom_id;

  IF is_daytime_calibration(oid) THEN
    IF NEW.c_observe_class <> 'dayCal' THEN
      RAISE EXCEPTION 'Step % of daytime calibration % must have observe class dayCal, not %', NEW.c_step_id, oid, NEW.c_observe_class;
    END IF;

    IF NEW.c_guide_state <> 'disabled' THEN
      RAISE EXCEPTION 'Step % of daytime calibration % must have guiding disabled', NEW.c_step_id, oid;
    END IF;
  END IF;

  RETURN NEW;

END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER trigger_t_step_daytime_calibration
  AFTER INSERT OR UPDATE ON t_step
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION check_daytime_calibration_step();

CREATE FUNCTION check_daytime_calibration_atom()
RETURNS TRIGGER AS $$
BEGIN

  IF NEW.c_sequence_type = 'acquisition' AND is_daytime_calibration(NEW.c_observation_id) THEN
    RAISE EXCEPTION 'Daytime calibration % cannot have an acquisition sequence', NEW.c_observation_id;
  END IF;

  RETURN NEW;

END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER trigger_t_atom_daytime_calibration
  AFTER INSERT OR UPDATE ON t_atom
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION check_daytime_calibration_atom();

CREATE FUNCTION check_daytime_calibration_observation()
RETURNS TRIGGER AS $$
BEGIN

  IF NEW.c_calibration_role = 'daytime_calibration' THEN
    IF EXISTS (
      SELECT 1
        FROM t_atom
       WHERE c_observation_id = NEW.c_observation_id
         AND c_sequence_type  = 'acquisition'
    ) THEN
      RAISE EXCEPTION 'Daytime calibration % cannot have an acquisition sequence', NEW.c_observation_id;
    END IF;

    IF EXISTS (
      SELECT 1
        FROM t_step s
        JOIN t_atom a ON a.c_atom_id = s.c_atom_id
       WHERE a.c_observation_id = NEW.c_observation_id
         AND (s.c_observe_class <> 'dayCal' OR s.c_guide_state <> 'disabled')
    ) THEN
      RAISE EXCEPTION 'Daytime calibration % has steps that are not dayCal with guiding disabled', NEW.c_observation_id;
    END IF;
  END IF;

  RETURN NEW;

END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER trigger_t_observation_daytime_calibration
  AFTER INSERT OR UPDATE OF c_calibration_role ON t_observation
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION check_daytime_calibration_observation();
