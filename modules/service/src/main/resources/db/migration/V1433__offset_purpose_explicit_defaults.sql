-- sc-10968: lucuma-core's default offset patterns now state each position's
-- purpose (object or sky) explicitly.  The GMOS MOS and IFU views duplicate
-- their defaults as JSON, so they are brought back in line.  The purpose
-- resolves the same either way; only the explicitly stated form changes.
--
-- The GMOS MOS mode keys (c_mode_key) also embed this default, for rows with no
-- stored pattern.  They are left as they are: rebuilding the generated columns
-- would mean dropping the views and the observing mode group union for a key
-- whose grouping the purpose does not affect.

-- From V1291, with the default position's purpose made explicit.
DROP VIEW v_gmos_north_mos;

CREATE VIEW v_gmos_north_mos AS
SELECT
  m.*,
  (
    SELECT f.c_tag
      FROM t_gmos_north_filter f
      WHERE f.c_is_acquisition_filter
      ORDER BY abs(f.c_wavelength - m.c_central_wavelength)
      LIMIT 1
  ) AS c_acquisition_filter_default,

  d.c_telescope_configs_default,
  COALESCE(m.c_telescope_configs, d.c_telescope_configs_default) AS c_telescope_configs_effective

FROM t_gmos_north_mos m
CROSS JOIN LATERAL (
  SELECT
    -- ATTENTION: duplicated from lucuma-core gmos.mos.DefaultTelescopeConfigs
    -- (a single guided position on target, explicitly on the object).
    -- Keep in sync.
    '[{"offset":{"p":{"microarcseconds":0},"q":{"microarcseconds":0}},"guiding":"ENABLED","explicitPurpose":"OBJECT"}]'::text AS c_telescope_configs_default
) d;

-- From V1291, with the default position's purpose made explicit.
DROP VIEW v_gmos_south_mos;

CREATE VIEW v_gmos_south_mos AS
SELECT
  m.*,
  (
    SELECT f.c_tag
      FROM t_gmos_south_filter f
      WHERE f.c_is_acquisition_filter
      ORDER BY abs(f.c_wavelength - m.c_central_wavelength)
      LIMIT 1
  ) AS c_acquisition_filter_default,

  d.c_telescope_configs_default,
  COALESCE(m.c_telescope_configs, d.c_telescope_configs_default) AS c_telescope_configs_effective

FROM t_gmos_south_mos m
CROSS JOIN LATERAL (
  SELECT
    -- ATTENTION: duplicated from lucuma-core gmos.mos.DefaultTelescopeConfigs
    -- (a single guided position on target, explicitly on the object).
    -- Keep in sync.
    '[{"offset":{"p":{"microarcseconds":0},"q":{"microarcseconds":0}},"guiding":"ENABLED","explicitPurpose":"OBJECT"}]'::text AS c_telescope_configs_default
) d;

-- From V1343, with the default position's purpose made explicit.
DROP VIEW v_gmos_north_ifu;

CREATE VIEW v_gmos_north_ifu AS
SELECT
  m.*,
  (
    SELECT f.c_tag
      FROM t_gmos_north_filter f
      WHERE f.c_is_acquisition_filter
      ORDER BY abs(f.c_wavelength - m.c_central_wavelength)
      LIMIT 1
  ) AS c_acquisition_filter_default,

  'Ccd2FullFrame'::e_gmos_ifu_acquisition_roi AS c_acquisition_roi_default,

  d.c_telescope_configs_default,
  COALESCE(m.c_telescope_configs, d.c_telescope_configs_default) AS c_telescope_configs_effective

FROM t_gmos_north_ifu m
CROSS JOIN LATERAL (
  SELECT
    -- ATTENTION: duplicated from gmos.ifu.Config.DefaultTelescopeConfigs (a single
    -- guided position on target, explicitly on the object; the IFU has a
    -- dedicated sky field so it does not nod).  Keep in sync.
    '[{"offset":{"p":{"microarcseconds":0},"q":{"microarcseconds":0}},"guiding":"ENABLED","explicitPurpose":"OBJECT"}]'::text AS c_telescope_configs_default
) d;

-- From V1343, with the default position's purpose made explicit.
DROP VIEW v_gmos_south_ifu;

CREATE VIEW v_gmos_south_ifu AS
SELECT
  m.*,
  (
    SELECT f.c_tag
      FROM t_gmos_south_filter f
      WHERE f.c_is_acquisition_filter
      ORDER BY abs(f.c_wavelength - m.c_central_wavelength)
      LIMIT 1
  ) AS c_acquisition_filter_default,

  'Ccd2FullFrame'::e_gmos_ifu_acquisition_roi AS c_acquisition_roi_default,

  d.c_telescope_configs_default,
  COALESCE(m.c_telescope_configs, d.c_telescope_configs_default) AS c_telescope_configs_effective

FROM t_gmos_south_ifu m
CROSS JOIN LATERAL (
  SELECT
    -- ATTENTION: duplicated from gmos.ifu.Config.DefaultTelescopeConfigs (a single
    -- guided position on target, explicitly on the object; the IFU has a
    -- dedicated sky field so it does not nod).  Keep in sync.
    '[{"offset":{"p":{"microarcseconds":0},"q":{"microarcseconds":0}},"guiding":"ENABLED","explicitPurpose":"OBJECT"}]'::text AS c_telescope_configs_default
) d;
