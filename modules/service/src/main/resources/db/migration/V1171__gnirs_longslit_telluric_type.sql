-- Add telluric type field to GNIRS Long Slit, defaults to HOT (matches F2/IGRINS-2).
ALTER TABLE t_gnirs_long_slit
ADD COLUMN c_telluric_type jsonb NOT NULL DEFAULT '{"tag":"HOT","starTypes":null}'::jsonb;

-- Recreate the view so the new c_telluric_type column (selected via ls.*) is exposed.
-- Definition is otherwise identical to V1165.
DROP VIEW v_gnirs_long_slit;

CREATE VIEW v_gnirs_long_slit AS
  SELECT
    ls.*,
    -- grating wavelength default + effective
    d.c_grating_wavelength_default,
    COALESCE(ls.c_grating_wavelength, d.c_grating_wavelength_default) AS c_grating_wavelength_effective,
    -- effective grating/prism: COALESCE(explicit, initial)
    COALESCE(ls.c_grating, ls.c_initial_grating) AS c_grating_effective,
    COALESCE(ls.c_prism,   ls.c_initial_prism)   AS c_prism_effective,
    -- decker: default, effective
    -- ATTENTION: This logic is duplicated from lucuma-core GnirsDecker. Modify in sync.
    d.c_decker_default,
    COALESCE(ls.c_decker, d.c_decker_default) AS c_decker_effective,
    -- well depth: default, effective
    -- ATTENTION: This logic is duplicated from lucuma-core GnirsWellDepth. Modify in sync.
    d.c_well_depth_default,
    COALESCE(ls.c_well_depth, d.c_well_depth_default) AS c_well_depth_effective,
    -- slit offset mode + telescope configs: default, effective
    d.c_slit_offset_mode_default,
    d.c_telescope_configs_default,
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
      -- decker default: mirrors GnirsDecker.forCameraAndReadMode(camera, effectivePrism)
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
      -- well depth default: mirrors GnirsWellDepth.forCamera
      (CASE
        WHEN ls.c_camera IN ('ShortBlue', 'LongBlue') THEN 'Shallow'
        WHEN ls.c_camera IN ('ShortRed',  'LongRed')  THEN 'Deep'
      END)::e_gnirs_well_depth AS c_well_depth_default,
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
