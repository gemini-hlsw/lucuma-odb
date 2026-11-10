-- Flamingos 2 long slit and MOS gain an explicit acquisition type and, for a
-- Faint acquisition, the offset at which its sky frames are taken. This follows
-- GNIRS (V1150, V1154, V1155, V1165): a NULL type means automatic, and the sky
-- offset is stored as two components that are both set exactly when the type is
-- Faint.

CREATE TYPE e_flamingos_2_acquisition_type AS ENUM ('Bright', 'Faint');

-- The views select m.*, whose expansion is frozen at creation, so they must be
-- rebuilt to pick up the new columns.
DROP VIEW v_flamingos_2_long_slit;
DROP VIEW v_flamingos_2_mos;

ALTER TABLE t_flamingos_2_long_slit
  ADD COLUMN c_acquisition_type         e_flamingos_2_acquisition_type,
  ADD COLUMN c_acquisition_sky_offset_p d_angle_µas,
  ADD COLUMN c_acquisition_sky_offset_q d_angle_µas,
  ADD CONSTRAINT acquisition_sky_offset_both_or_neither
    CHECK ((c_acquisition_sky_offset_p IS NULL) = (c_acquisition_sky_offset_q IS NULL)),
  ADD CONSTRAINT acquisition_sky_offset_faint_only
    CHECK ((c_acquisition_sky_offset_p IS NOT NULL) = (c_acquisition_type IS NOT DISTINCT FROM 'Faint'));

ALTER TABLE t_flamingos_2_mos
  ADD COLUMN c_acquisition_type         e_flamingos_2_acquisition_type,
  ADD COLUMN c_acquisition_sky_offset_p d_angle_µas,
  ADD COLUMN c_acquisition_sky_offset_q d_angle_µas,
  ADD CONSTRAINT acquisition_sky_offset_both_or_neither
    CHECK ((c_acquisition_sky_offset_p IS NULL) = (c_acquisition_sky_offset_q IS NULL)),
  ADD CONSTRAINT acquisition_sky_offset_faint_only
    CHECK ((c_acquisition_sky_offset_p IS NOT NULL) = (c_acquisition_type IS NOT DISTINCT FROM 'Faint'));

-- Body identical to V1325.
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

-- Body identical to V1265.
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
      'nod_along_slit'::varchar AS c_slit_offset_mode_default,
      -- Default telescope configs JSON (transport codec).
      -- ATTENTION: duplicated from lucuma-core flamingos2.defaultMosTelescopeConfigs
      -- for the sparse field. Keep in sync.
      '[{"q":{"microarcseconds":1200000},"guiding":"ENABLED"},{"q":{"microarcseconds":-1200000},"guiding":"ENABLED"},{"q":{"microarcseconds":-1200000},"guiding":"ENABLED"},{"q":{"microarcseconds":1200000},"guiding":"ENABLED"}]'::text AS c_telescope_configs_default
  ) d;
