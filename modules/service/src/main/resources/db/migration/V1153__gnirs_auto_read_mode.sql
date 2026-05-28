-- GNIRS science read mode: switch from e_gnirs_obs_read_mode (which carried an
-- 'Automatic' sentinel) to a nullable GnirsReadMode tag, mirroring Flamingos2.
-- A NULL c_read_mode now means "Auto": the read mode is computed from the step
-- exposure time at sequence-generation time. The four concrete read modes are
-- stored as their tag, exactly like the materialized sequence's c_read_mode.

-- Drop the view first; it expands ls.* at creation time so it pins both the
-- c_read_mode column type and the c_read_mode_default expression.
DROP VIEW v_gnirs_long_slit;

-- Convert the explicit override column: the 'Automatic' sentinel collapses to
-- NULL (i.e. no explicit override → compute from exposure time); the concrete
-- read modes keep their tag in a d_tag column.
ALTER TABLE t_gnirs_long_slit
  ALTER COLUMN c_read_mode TYPE d_tag
  USING (CASE WHEN c_read_mode = 'Automatic' THEN NULL ELSE c_read_mode::text END)::d_tag;

-- Recreate v_gnirs_long_slit — body identical to V1150 except the now-removed
-- c_read_mode_default column (there is no longer a stored default; the effective
-- read mode is resolved in Scala from the exposure time, like Flamingos2).
CREATE VIEW v_gnirs_long_slit AS
  SELECT
    ls.*,
    -- decker pure default: mirrors GnirsDecker.forCameraAndReadMode(camera, effectivePrism)
    -- ATTENTION: This logic is duplicated from lucuma-core GnirsDecker. Modify in sync.
    (CASE
      WHEN COALESCE(ls.c_prism, ls.c_initial_prism) = 'Mirror' THEN
        CASE WHEN ls.c_camera IN ('ShortRed', 'ShortBlue') THEN 'ShortCamLongSlit'
             ELSE 'LongCamLongSlit'
        END
      ELSE -- Sxd or Lxd
        CASE WHEN ls.c_camera IN ('ShortRed', 'ShortBlue') THEN 'ShortCamCrossDispersed'
             ELSE 'LongCamCrossDispersed'
        END
    END)::e_gnirs_decker AS c_decker_default,
    -- grating wavelength default (computed once in lateral, referenced here and in effective)
    d.c_grating_wavelength_default,
    COALESCE(ls.c_grating_wavelength, d.c_grating_wavelength_default) AS c_grating_wavelength_effective,
    -- well depth pure default: mirrors GnirsWellDepth.forCamera
    (CASE
      WHEN ls.c_camera IN ('ShortBlue', 'LongBlue') THEN 'Shallow'
      WHEN ls.c_camera IN ('ShortRed',  'LongRed')  THEN 'Deep'
    END)::e_gnirs_well_depth AS c_well_depth_default,
    -- effective grating/prism: COALESCE(explicit, initial)
    COALESCE(ls.c_grating, ls.c_initial_grating) AS c_grating_effective,
    COALESCE(ls.c_prism,   ls.c_initial_prism)   AS c_prism_effective,
    -- slit offset mode default (always nod_along_slit for GNIRS)
    d.c_slit_offset_mode_default,
    -- telescope configs default (computed once in lateral, referenced here and in effective)
    d.c_telescope_configs_default,
    -- effective = COALESCE(explicit, default)
    COALESCE(ls.c_slit_offset_mode, d.c_slit_offset_mode_default) AS c_slit_offset_mode_effective,
    COALESCE(ls.c_telescope_configs, d.c_telescope_configs_default) AS c_telescope_configs_effective
  FROM t_gnirs_long_slit ls
  CROSS JOIN LATERAL (
    SELECT
      -- slit offset mode default (always nod_along_slit for GNIRS)
      'nod_along_slit'::varchar AS c_slit_offset_mode_default,
      -- grating wavelength default: filter.optimalWavelength in pm
      CASE ls.c_filter
        WHEN 'Order6'   THEN 1100000
        WHEN 'Order5'   THEN 1270000
        WHEN 'Order4'   THEN 1645000
        WHEN 'Order3'   THEN 2200000
        WHEN 'Order2'   THEN 3500000
        WHEN 'Order1'   THEN 4800000
        ELSE 1650000
      END AS c_grating_wavelength_default,
      -- telescope configs default:
      -- Cross-dispersed prisms → [-1", +2", +2", -1"] along slit
      -- Short camera long slit  → [+2", -4", -4", +2"] along slit
      -- Long camera, filter >= 2.5 µm (Order2/Order1/PAH) → [-3", +3", +3", -3"] along slit
      -- Long camera, filter < 2.5 µm (all others)         → [-1", +5", +5", -1"] along slit
      CASE
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

-- The 'Automatic' sentinel type is no longer used anywhere (the acquisition read
-- mode was already replaced by e_gnirs_acquisition_type in V1150).
DROP TYPE e_gnirs_obs_read_mode;
