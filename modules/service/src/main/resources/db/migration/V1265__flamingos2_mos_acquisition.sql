-- Flamingos 2 MOS gains an acquisition sequence, so it now carries the same
-- acquisition configuration long slit does: an optional filter override (the
-- default being the acquisition filter nearest the science filter) and an
-- acquisition-role exposure time mode.

ALTER TABLE t_flamingos_2_mos
  ADD COLUMN c_acquisition_filter d_tag REFERENCES t_f2_filter(c_tag);

-- Rebuild the view with the acquisition filter default, computed exactly as in
-- v_flamingos_2_long_slit (V1219).  The preset-driven telescope config defaults
-- are unchanged from V1264.
DROP VIEW v_flamingos_2_mos;

CREATE VIEW v_flamingos_2_mos AS
  SELECT
    m.*,
    (
      SELECT af.c_tag
        FROM t_f2_filter af
        JOIN t_f2_filter sf ON sf.c_tag = m.c_filter
        WHERE af.c_is_acquisition_filter
        ORDER BY abs(af.c_wavelength - sf.c_wavelength)
        LIMIT 1
    ) AS c_acquisition_filter_default,
    d.c_slit_offset_mode_default,
    d.c_telescope_configs_default,
    COALESCE(m.c_slit_offset_mode,  d.c_slit_offset_mode_default)  AS c_slit_offset_mode_effective,
    COALESCE(m.c_telescope_configs, d.c_telescope_configs_default) AS c_telescope_configs_effective
  FROM t_flamingos_2_mos m
  CROSS JOIN LATERAL (
    SELECT
      -- Default slit offset mode (shape) per preset.
      CASE m.c_mos_offset_preset
        WHEN 'crowded_field' THEN 'nod_to_sky'
        ELSE                      'nod_along_slit'
      END::varchar AS c_slit_offset_mode_default,
      -- Default telescope configs JSON (transport codec).
      -- ATTENTION: duplicated from lucuma-core flamingos2.defaultMosTelescopeConfigs.
      -- Keep in sync.
      CASE m.c_mos_offset_preset
        WHEN 'crowded_field' THEN
          '[{"offset":{"p":{"microarcseconds":0},"q":{"microarcseconds":0}},"guiding":"ENABLED"},{"offset":{"p":{"microarcseconds":0},"q":{"microarcseconds":300000000}},"guiding":"DISABLED"},{"offset":{"p":{"microarcseconds":0},"q":{"microarcseconds":320000000}},"guiding":"DISABLED"},{"offset":{"p":{"microarcseconds":0},"q":{"microarcseconds":0}},"guiding":"ENABLED"}]'
        ELSE
          '[{"q":{"microarcseconds":1200000},"guiding":"ENABLED"},{"q":{"microarcseconds":-1200000},"guiding":"ENABLED"},{"q":{"microarcseconds":-1200000},"guiding":"ENABLED"},{"q":{"microarcseconds":1200000},"guiding":"ENABLED"}]'
      END::text AS c_telescope_configs_default
  ) d;

-- Existing F2 MOS observations have no acquisition ETM row.  Give each one the
-- same default the create path would have produced: signal-to-noise 10 at the
-- science ETM's wavelength (see ExposureTimeMode.forAcquisition).
INSERT INTO t_exposure_time_mode (
  c_observation_id,
  c_role,
  c_exposure_time_mode,
  c_signal_to_noise,
  c_signal_to_noise_at
)
SELECT
  sci.c_observation_id,
  'acquisition',
  'signal_to_noise',
  10,
  sci.c_signal_to_noise_at
FROM t_exposure_time_mode sci
JOIN t_observation o ON o.c_observation_id = sci.c_observation_id
WHERE o.c_observing_mode_type = 'flamingos_2_mos'
  AND sci.c_role = 'science'
  AND NOT EXISTS (
    SELECT 1
      FROM t_exposure_time_mode acq
     WHERE acq.c_observation_id = sci.c_observation_id
       AND acq.c_role = 'acquisition'
  );

-- F2 MOS now requires exactly one acquisition ETM, like long slit.  Unlike GMOS
-- MOS it accepts any exposure time mode, since it has an acquisition ITC pass.
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
      WHEN obs_mode IN ('flamingos_2_long_slit', 'flamingos_2_mos', 'gmos_north_long_slit', 'gmos_south_long_slit') THEN
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

-- Every existing F2 MOS observation's sequence grows an acquisition, so its
-- digest changes.
UPDATE t_obscalc c
   SET c_obscalc_state = 'pending'
  FROM t_observation o
 WHERE c.c_observation_id = o.c_observation_id
   AND o.c_observing_mode_type = 'flamingos_2_mos';
