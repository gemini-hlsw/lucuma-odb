-- GMOS MOS gains an acquisition configuration: an optional explicit
-- acquisition filter  and an acquisition exposure time mode.

-- The acquisition filter column.
ALTER TABLE t_gmos_north_mos
  ADD COLUMN c_acquisition_filter d_tag REFERENCES t_gmos_north_filter(c_tag);

ALTER TABLE t_gmos_south_mos
  ADD COLUMN c_acquisition_filter d_tag REFERENCES t_gmos_south_filter(c_tag);

-- Recreate the views to expose the wavelength-nearest default acquisition
-- filter, computed exactly as long slit computes it.
DROP VIEW v_gmos_north_mos;
DROP VIEW v_gmos_south_mos;

CREATE VIEW v_gmos_north_mos AS
SELECT
  m.*,
  (
    SELECT f.c_tag
      FROM t_gmos_north_filter f
      WHERE f.c_is_acquisition_filter
      ORDER BY abs(f.c_wavelength - m.c_central_wavelength)
      LIMIT 1
  ) AS c_acquisition_filter_default
FROM t_gmos_north_mos m;

CREATE VIEW v_gmos_south_mos AS
SELECT
  m.*,
  (
    SELECT f.c_tag
      FROM t_gmos_south_filter f
      WHERE f.c_is_acquisition_filter
      ORDER BY abs(f.c_wavelength - m.c_central_wavelength)
      LIMIT 1
  ) AS c_acquisition_filter_default
FROM t_gmos_south_mos m;

-- Validate that an explicit acquisition filter is one of the acquisition
-- filters, reusing the function long slit already defines (V1045).
CREATE TRIGGER check_gmos_north_mos_acquisition_filter_is_valid_trigger
BEFORE INSERT OR UPDATE OF c_acquisition_filter
ON t_gmos_north_mos
FOR EACH ROW
EXECUTE FUNCTION check_gmos_acquisition_filter_is_valid('t_gmos_north_filter');

CREATE TRIGGER check_gmos_south_mos_acquisition_filter_is_valid_trigger
BEFORE INSERT OR UPDATE OF c_acquisition_filter
ON t_gmos_south_mos
FOR EACH ROW
EXECUTE FUNCTION check_gmos_acquisition_filter_is_valid('t_gmos_south_filter');

-- MOS requires an acquisition etm of type T&C
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
      WHEN obs_mode IN ('flamingos_2_long_slit', 'gmos_north_long_slit', 'gmos_south_long_slit', 'gnirs_long_slit', 'gnirs_ifu') THEN
        IF acq_count <> 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have an acquisition exposure time mode', obs_id, obs_mode;
        END IF;

        IF sci_count <> 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have exactly one science exposure time mode', obs_id, obs_mode;
        END IF;

      WHEN obs_mode IN ('gmos_north_mos', 'gmos_south_mos') THEN
        IF acq_count <> 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have an acquisition exposure time mode', obs_id, obs_mode;
        END IF;

        IF sci_count <> 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have exactly one science exposure time mode', obs_id, obs_mode;
        END IF;

        -- The acquisition mode must be Time & Count: there is no acquisition ITC
        -- pass to solve a signal-to-noise one.
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

      WHEN obs_mode IN ('gmos_north_imaging', 'gmos_south_imaging', 'flamingos_2_imaging', 'gnirs_imaging') THEN
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

      WHEN obs_mode IN ('exchange_keck', 'exchange_subaru') THEN
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
