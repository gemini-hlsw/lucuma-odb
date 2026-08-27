-- A Flamingos 2 MOS telluric is observed through the builtin long slit matching
-- the mask's slitlets, so its calibration observation is a long slit one.  Its
-- sequence still differs from a long slit observation's own telluric, so the
-- origin has to be readable at generation time.

ALTER TABLE t_flamingos_2_long_slit
  ADD COLUMN c_telluric_science_mode e_observing_mode_type NULL DEFAULT NULL,
  -- MOS is the only mode that calibrates as a different one.
  ADD CONSTRAINT flamingos_2_long_slit_telluric_science_mode_check
    CHECK (c_telluric_science_mode IS NULL OR c_telluric_science_mode = 'flamingos_2_mos'),
  -- Only a telluric is derived from another mode.
  ADD CONSTRAINT flamingos_2_long_slit_telluric_science_mode_role_check
    CHECK (c_telluric_science_mode IS NULL OR is_telluric_calibration(c_observation_id));

COMMENT ON COLUMN t_flamingos_2_long_slit.c_telluric_science_mode IS
  'For a telluric derived from another observing mode, that mode.  Null otherwise.';

-- The view selects m.*, whose expansion is frozen at creation, and the new column
-- lands among the existing ones rather than after them, so CREATE OR REPLACE will
-- not do.
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
      -- ATTENTION: duplicated from lucuma-core flamingos2.defaultSlitTelescopeConfigs
      -- (Flamingos2SlitOffsetPreset.Telluric). Keep in sync.
      '[{"q":{"microarcseconds":15000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-15000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-15000000},"guiding":"ENABLED"},{"q":{"microarcseconds":15000000},"guiding":"ENABLED"}]' AS c_telescope_configs_default
  ) d;
