
-- Fix instruments
DELETE FROM t_instrument WHERE c_tag IN ('AcqCam', 'Visitor');
INSERT INTO t_instrument VALUES
  ('AcqCamNorth', 'AcqCam (North)', 'Acquisition Camera (North)'),
  ('AcqCamSouth', 'AcqCam (South)', 'Acquisition Camera (South)'),
  ('VisitorNorth', 'Visitor (North)', 'Visitor Instrument (North)'),
  ('VisitorSouth', 'Visitor (South)', 'Visitor Instrument (South)'),
  ('MaroonX', 'MAROON-X', 'MAROON-X');

-- Add visitor modes
ALTER TYPE e_observing_mode_type ADD VALUE 'alopeke_speckle';
ALTER TYPE e_observing_mode_type ADD VALUE 'alopeke_wide_field';
ALTER TYPE e_observing_mode_type ADD VALUE 'zorro_speckle';
ALTER TYPE e_observing_mode_type ADD VALUE 'zorro_wide_field';
ALTER TYPE e_observing_mode_type ADD VALUE 'visitor_north';
ALTER TYPE e_observing_mode_type ADD VALUE 'visitor_south';
ALTER TYPE e_observing_mode_type ADD VALUE 'maroon_x';

-- Add oid, mode key so we can refer to it
CREATE UNIQUE INDEX ON t_observation (c_observation_id, c_observing_mode_type);

-- Add visitor config table
CREATE TABLE t_visitor (

  c_observation_id             d_observation_id      NOT NULL,
  c_observing_mode_type        e_observing_mode_type NOT NULL 
    CHECK (
      c_observing_mode_type IN (
        'alopeke_speckle',
        'alopeke_wide_field',
        'zorro_speckle',
        'zorro_wide_field',
        'visitor_north',
        'visitor_south',
        'maroon_x'
      )
    ),

  c_central_wavelength         d_wavelength_pm  NOT NULL,
  c_guide_star_min_sep         d_angle_µas      NOT NULL,

  PRIMARY KEY (c_observation_id, c_observing_mode_type),
  FOREIGN KEY (c_observation_id, c_observing_mode_type) REFERENCES t_observation(c_observation_id, c_observing_mode_type) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED
);

-- Update check_etm_consistent to handle ghost_ifu
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
      WHEN obs_mode IN ('flamingos_2_long_slit', 'gmos_north_long_slit', 'gmos_south_long_slit') THEN
        IF acq_count <> 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have an acquisition exposure time mode', obs_id, obs_mode;
        END IF;

        IF sci_count <> 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have exactly one science exposure time mode', obs_id, obs_mode;
        END IF;

      WHEN obs_mode = 'ghost_ifu' THEN
        IF sci_count <> 2 THEN
          RAISE EXCEPTION 'Observation % with mode % must have two science exposure time modes (red and blue camera)', obs_id, obs_mode;
        END IF;

      WHEN obs_mode = 'igrins_2_long_slit' THEN
        IF sci_count <> 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have exactly one science exposure time mode', obs_id, obs_mode;
        END IF;

      WHEN obs_mode IN ('gmos_north_imaging', 'gmos_south_imaging') THEN
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

      ELSE
        RAISE EXCEPTION 'Unknown observing mode % for observation %', obs_mode, obs_id;
    END CASE;
  END IF;

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION instrument_reference_name(
  instrument d_tag
)
RETURNS text AS $$
DECLARE
  name text;
BEGIN
  name := CASE
    WHEN instrument = 'AcqCamNorth'  THEN 'ACQCAMN'
    WHEN instrument = 'AcqCamSouth'  THEN 'ACQCAMS'
    WHEN instrument = 'Alopeke'      THEN 'ALOPEKE'
    WHEN instrument = 'Flamingos2'   THEN 'F2'
    WHEN instrument = 'Ghost'        THEN 'GHOST'
    WHEN instrument = 'GmosNorth'    THEN 'GMOSN'
    WHEN instrument = 'GmosSouth'    THEN 'GMOSS'
    WHEN instrument = 'Gnirs'        THEN 'GNIRS'
    WHEN instrument = 'Gpi'          THEN 'GPI'
    WHEN instrument = 'Gsaoi'        THEN 'GSAOI'
    WHEN instrument = 'Igrins2'      THEN 'IGRINS2'
    WHEN instrument = 'MaroonX'      THEN 'MAROONX'
    WHEN instrument = 'Niri'         THEN 'NIRI'
    WHEN instrument = 'Scorpio'      THEN 'SCORPIO'
    WHEN instrument = 'VisitorNorth' THEN 'VISITORN'
    WHEN instrument = 'VisitorSouth' THEN 'VISITORS'
    WHEN instrument = 'Zorro'        THEN 'ZORRO'
  END;

  IF name IS NULL THEN
    RAISE EXCEPTION 'Unknown instrument: %', instrument::text
      USING HINT = 'Please update instrument_reference_name function';
  END IF;

  RETURN name;
END;
$$ LANGUAGE plpgsql IMMUTABLE;


