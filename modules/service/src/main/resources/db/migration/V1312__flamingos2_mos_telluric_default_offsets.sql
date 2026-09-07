-- A Flamingos 2 MOS telluric's default slit offsets are the MOS telluric nod
-- pattern (stepping the standard down the slit), not the long slit telluric's
-- fixed ±15" nod (these values used to live in
-- Flamingos2LongSlitService.applyF2MosTelluricDefaults, which wrote them into
-- the explicit columns). Left as an explicit override, the API reported the
-- MOS telluric as customizing a long slit default it never actually chose,
-- and reverting it restored the ±15" pattern. Make the view's default
-- role-aware instead, so a MOS telluric's own pattern is the default and
-- nothing need be stored explicitly.

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
        WHEN m.c_telluric_science_mode = 'flamingos_2_mos' THEN
          -- ATTENTION: duplicated from lucuma-core flamingos2.defaultSlitTelescopeConfigs
          -- (Flamingos2SlitOffsetPreset.MosTelluric). Keep in sync.
          '[{"q":{"microarcseconds":60000000},"guiding":"ENABLED"},{"q":{"microarcseconds":40000000},"guiding":"ENABLED"},{"q":{"microarcseconds":20000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-20000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-40000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-60000000},"guiding":"ENABLED"}]'
        ELSE
          -- ATTENTION: duplicated from lucuma-core flamingos2.defaultSlitTelescopeConfigs
          -- (Flamingos2SlitOffsetPreset.Telluric). Keep in sync.
          '[{"q":{"microarcseconds":15000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-15000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-15000000},"guiding":"ENABLED"},{"q":{"microarcseconds":15000000},"guiding":"ENABLED"}]'
      END AS c_telescope_configs_default
  ) d;

-- Existing MOS tellurics whose explicit offsets just duplicate the (now
-- role-aware) default were only ever set by applyF2MosTelluricDefaults, never
-- hand-edited, so null them out and let them fall back to the default like new
-- ones do. A telluric whose explicit value differs from the pattern was edited
-- by hand and is left alone.
UPDATE t_flamingos_2_long_slit m
SET c_slit_offset_mode = NULL, c_telescope_configs = NULL
FROM v_flamingos_2_long_slit v
WHERE v.c_observation_id = m.c_observation_id
  AND m.c_telluric_science_mode = 'flamingos_2_mos'
  AND m.c_slit_offset_mode = v.c_slit_offset_mode_default
  AND m.c_telescope_configs::jsonb = v.c_telescope_configs_default::jsonb;
