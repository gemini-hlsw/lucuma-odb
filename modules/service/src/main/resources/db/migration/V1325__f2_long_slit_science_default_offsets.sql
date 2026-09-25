-- Flamingos 2 long slit science observations default to the NodAlongSlit
-- pattern. Tellurics keep their existing defaults.

-- The view selects m.*, whose expansion is frozen at creation, so CREATE OR
-- REPLACE will not do.
DROP VIEW v_flamingos_2_long_slit;

CREATE VIEW v_flamingos_2_long_slit AS
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
  FROM t_flamingos_2_long_slit m
  CROSS JOIN LATERAL (
    SELECT
      'nod_along_slit'::varchar AS c_slit_offset_mode_default,
      CASE
        WHEN NOT is_telluric_calibration(m.c_observation_id) THEN
          -- ATTENTION: duplicated from lucuma-core flamingos2.defaultSlitTelescopeConfigs
          -- (Flamingos2SlitOffsetPreset.NodAlongSlit). Keep in sync.
          '[{"q":{"microarcseconds":10000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-10000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-10000000},"guiding":"ENABLED"},{"q":{"microarcseconds":10000000},"guiding":"ENABLED"}]'
        WHEN m.c_telluric_science_mode = 'flamingos_2_mos' THEN
          -- ATTENTION: duplicated from lucuma-core flamingos2.defaultTelluricTelescopeConfigs
          -- (Flamingos2TelluricOffsetPreset.MosTelluric). Keep in sync.
          '[{"q":{"microarcseconds":60000000},"guiding":"ENABLED"},{"q":{"microarcseconds":40000000},"guiding":"ENABLED"},{"q":{"microarcseconds":20000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-20000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-40000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-60000000},"guiding":"ENABLED"}]'
        ELSE
          -- ATTENTION: duplicated from lucuma-core flamingos2.defaultTelluricTelescopeConfigs
          -- (Flamingos2TelluricOffsetPreset.Telluric). Keep in sync.
          '[{"q":{"microarcseconds":15000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-15000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-15000000},"guiding":"ENABLED"},{"q":{"microarcseconds":15000000},"guiding":"ENABLED"}]'
      END AS c_telescope_configs_default
  ) d;

-- Science observations without an override now resolve to a new default, but
-- replacing the view fires no invalidation trigger, so recalculate them.
DO $$
DECLARE
  obs_id d_observation_id;
BEGIN
  FOR obs_id IN
    SELECT m.c_observation_id
    FROM   t_flamingos_2_long_slit m
    JOIN   t_observation o ON o.c_observation_id = m.c_observation_id
    WHERE  m.c_telescope_configs IS NULL
      AND  o.c_existence = 'present'
      AND  NOT is_telluric_calibration(m.c_observation_id)
  LOOP
    CALL invalidate_obscalc(obs_id);
  END LOOP;
END;
$$;
