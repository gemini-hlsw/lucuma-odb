-- GNIRS spectroscopy observations may take spectra at several central wavelengths.
-- Each central wavelength is a separate configuration with its own exposure time
-- mode, coadds, ITC calculation and smart calibrations, so the single
-- (c_central_wavelength, c_initial_central_wavelength, c_coadds) triple on
-- t_gnirs_spectroscopy moves to a child table with one row per wavelength,
-- modeled after t_gnirs_imaging_filter.

CREATE TABLE t_gnirs_spectroscopy_wavelength (
  c_observation_id        d_observation_id             NOT NULL,
  c_central_wavelength    d_wavelength_pm              NOT NULL,
  c_version               e_observing_mode_row_version NOT NULL DEFAULT 'current',
  c_coadds                int4                         NOT NULL DEFAULT 1 CHECK (c_coadds > 0),
  c_exposure_time_mode_id integer                      NOT NULL,
  c_role                  e_exposure_time_mode_role    NOT NULL DEFAULT 'science' CHECK (c_role = 'science'),

  PRIMARY KEY (c_observation_id, c_central_wavelength, c_version),
  CONSTRAINT t_gnirs_spectroscopy_wavelength_unique_exposure_time_mode_id
    UNIQUE (c_exposure_time_mode_id),
  FOREIGN KEY (c_observation_id)
    REFERENCES t_gnirs_spectroscopy(c_observation_id) ON DELETE CASCADE,
  FOREIGN KEY (c_exposure_time_mode_id, c_role)
    REFERENCES t_exposure_time_mode(c_exposure_time_mode_id, c_role)
    ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED
);

COMMENT ON TABLE t_gnirs_spectroscopy_wavelength IS
  'GNIRS spectroscopy central wavelengths, each with its own exposure time mode and coadds';

-- Backfill.  Existing observations have exactly one science ETM, which becomes the
-- 'current' row's ETM.  The 'initial' row needs its own ETM (the unique constraint
-- above allows an ETM to back only one wavelength row), so clone it.
INSERT INTO t_gnirs_spectroscopy_wavelength (
  c_observation_id,
  c_central_wavelength,
  c_version,
  c_coadds,
  c_exposure_time_mode_id
)
SELECT
  ls.c_observation_id,
  COALESCE(ls.c_central_wavelength, ls.c_initial_central_wavelength),
  'current'::e_observing_mode_row_version,
  ls.c_coadds,
  m.c_exposure_time_mode_id
FROM t_gnirs_spectroscopy ls
JOIN t_exposure_time_mode m
  ON m.c_observation_id = ls.c_observation_id
 AND m.c_role = 'science'::e_exposure_time_mode_role;

WITH cloned AS (
  INSERT INTO t_exposure_time_mode (
    c_observation_id,
    c_role,
    c_exposure_time_mode,
    c_signal_to_noise,
    c_signal_to_noise_at,
    c_exposure_time,
    c_exposure_count
  )
  SELECT
    m.c_observation_id,
    m.c_role,
    m.c_exposure_time_mode,
    m.c_signal_to_noise,
    m.c_signal_to_noise_at,
    m.c_exposure_time,
    m.c_exposure_count
  FROM t_gnirs_spectroscopy ls
  JOIN t_exposure_time_mode m
    ON m.c_observation_id = ls.c_observation_id
   AND m.c_role = 'science'::e_exposure_time_mode_role
  RETURNING c_exposure_time_mode_id, c_observation_id
)
INSERT INTO t_gnirs_spectroscopy_wavelength (
  c_observation_id,
  c_central_wavelength,
  c_version,
  c_coadds,
  c_exposure_time_mode_id
)
SELECT
  ls.c_observation_id,
  ls.c_initial_central_wavelength,
  'initial'::e_observing_mode_row_version,
  ls.c_coadds,
  cloned.c_exposure_time_mode_id
FROM t_gnirs_spectroscopy ls
JOIN cloned ON cloned.c_observation_id = ls.c_observation_id;

-- The wavelength and coadds now live in the child table.
DROP VIEW v_gnirs_spectroscopy;

ALTER TABLE t_gnirs_spectroscopy
  DROP COLUMN c_central_wavelength,
  DROP COLUMN c_initial_central_wavelength,
  DROP COLUMN c_coadds;

CREATE VIEW v_gnirs_spectroscopy AS
  SELECT
    ls.*,
    COALESCE(ls.c_grating, ls.c_initial_grating) AS c_grating_effective,
    COALESCE(ls.c_prism,   ls.c_initial_prism)   AS c_prism_effective,
    -- ATTENTION: This logic is duplicated from lucuma-core GnirsDecker. Modify in sync.
    d.c_decker_default,
    COALESCE(ls.c_decker, d.c_decker_default) AS c_decker_effective,
    -- ATTENTION: This logic is duplicated from lucuma-core GnirsWellDepth. Modify in sync.
    d.c_well_depth_default,
    COALESCE(ls.c_well_depth, d.c_well_depth_default) AS c_well_depth_effective,
    d.c_slit_offset_mode_default,
    d.c_telescope_configs_default,
    COALESCE(ls.c_slit_offset_mode, d.c_slit_offset_mode_default) AS c_slit_offset_mode_effective,
    COALESCE(ls.c_telescope_configs, d.c_telescope_configs_default) AS c_telescope_configs_effective
  FROM t_gnirs_spectroscopy ls
  CROSS JOIN LATERAL (
    SELECT
      -- IFU has no slit offset mode.
      (CASE WHEN ls.c_observing_mode_type = 'gnirs_ifu' THEN NULL ELSE 'nod_along_slit' END)::varchar
        AS c_slit_offset_mode_default,
      (CASE
        WHEN ls.c_fpu_ifu = 'LowResolution'  THEN 'LowResolutionIfu'
        WHEN ls.c_fpu_ifu = 'HighResolution' THEN 'HighResolutionIfu'
        WHEN COALESCE(ls.c_prism, ls.c_initial_prism) = 'Mirror' THEN
          CASE WHEN ls.c_camera IN ('ShortRed', 'ShortBlue') THEN 'ShortCamLongSlit'
               ELSE 'LongCamLongSlit'
          END
        ELSE -- Sxd or Lxd
          CASE WHEN ls.c_camera IN ('ShortRed', 'ShortBlue') THEN 'ShortCamCrossDispersed'
               ELSE 'LongCamCrossDispersed'
          END
      END)::e_gnirs_decker AS c_decker_default,
      (CASE
        WHEN ls.c_camera IN ('ShortBlue', 'LongBlue') THEN 'Shallow'
        WHEN ls.c_camera IN ('ShortRed',  'LongRed')  THEN 'Deep'
      END)::e_gnirs_well_depth AS c_well_depth_default,
      -- IFU telescope configs have no derived default (seeded at creation); only long slit.
      CASE
        WHEN ls.c_observing_mode_type = 'gnirs_ifu' THEN NULL
        WHEN COALESCE(ls.c_prism, ls.c_initial_prism) IN ('Sxd', 'Lxd') THEN
          '[{"q":{"microarcseconds":-1000000},"guiding":"ENABLED"},{"q":{"microarcseconds":2000000},"guiding":"ENABLED"},{"q":{"microarcseconds":2000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-1000000},"guiding":"ENABLED"}]'
        WHEN ls.c_camera IN ('ShortBlue', 'ShortRed') THEN
          '[{"q":{"microarcseconds":2000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-4000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-4000000},"guiding":"ENABLED"},{"q":{"microarcseconds":2000000},"guiding":"ENABLED"}]'
        WHEN ls.c_filter IN ('Order2', 'Order1', 'PAH') THEN
          '[{"q":{"microarcseconds":-3000000},"guiding":"ENABLED"},{"q":{"microarcseconds":3000000},"guiding":"ENABLED"},{"q":{"microarcseconds":3000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-3000000},"guiding":"ENABLED"}]'
        ELSE
          '[{"q":{"microarcseconds":-1000000},"guiding":"ENABLED"},{"q":{"microarcseconds":5000000},"guiding":"ENABLED"},{"q":{"microarcseconds":5000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-1000000},"guiding":"ENABLED"}]'
      END AS c_telescope_configs_default
  ) d;

-- Update check_etm_consistent.  GNIRS spectroscopy now has per-wavelength science
-- ETMs, in both the 'initial' and 'current' versions, so the science count is no
-- longer fixed -- exactly as for the imaging modes.  The acquisition ETM is still
-- required, and still exactly one.
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

-- The stored ITC science results for GNIRS spectroscopy change shape (one result
-- set per central wavelength), so cached, unfrozen results can no longer be decoded.
DELETE FROM t_itc_result WHERE NOT c_is_frozen;
