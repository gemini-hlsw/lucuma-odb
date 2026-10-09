-- Spectrophotometric standards now use the same acquisition ROI default as
-- science (CCD2 + Full Frame), so the default no longer depends on the role.

-- Applied to both GMOS-N and GMOS-S IFU tables, since the default is the same for both.
-- It is basically a copy of migration V1297 with a different default for the acquisition ROI.

CREATE OR REPLACE VIEW v_gmos_north_ifu AS
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
    -- guided position on target; the IFU has a dedicated sky field so it does not
    -- nod).  Keep in sync.
    '[{"offset":{"p":{"microarcseconds":0},"q":{"microarcseconds":0}},"guiding":"ENABLED"}]'::text AS c_telescope_configs_default
) d;

CREATE OR REPLACE VIEW v_gmos_south_ifu AS
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
    -- guided position on target; the IFU has a dedicated sky field so it does not
    -- nod).  Keep in sync.
    '[{"offset":{"p":{"microarcseconds":0},"q":{"microarcseconds":0}},"guiding":"ENABLED"}]'::text AS c_telescope_configs_default
) d;
