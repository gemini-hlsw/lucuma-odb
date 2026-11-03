-- The cross-dispersed decker must match the prism, not the camera.
--
-- The long camera can be paired with the short cross-dispersing prism (the
-- "LC SXD" Phase 0 configurations), and that setup uses the short camera XD
-- decker. The default previously followed the camera, defaulting LC SXD to
-- the long camera XD decker. Long slit (mirror) deckers still follow the
-- camera.
--
-- The view selects ls.*, whose expansion is frozen at creation, so CREATE OR
-- REPLACE will not do.
DROP VIEW v_gnirs_spectroscopy;

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
  LEFT JOIN t_observation o ON o.c_observation_id = ls.c_observation_id
  CROSS JOIN LATERAL (
    SELECT
      -- IFU has no slit offset mode.
      (CASE WHEN ls.c_observing_mode_type = 'gnirs_ifu' THEN NULL ELSE 'nod_along_slit' END)::varchar
        AS c_slit_offset_mode_default,
      -- Long slit deckers follow the camera; cross-dispersed deckers follow the prism.
      (CASE
        WHEN ls.c_fpu_ifu = 'LowResolution'  THEN 'LowResolutionIfu'
        WHEN ls.c_fpu_ifu = 'HighResolution' THEN 'HighResolutionIfu'
        WHEN COALESCE(ls.c_prism, ls.c_initial_prism) = 'Mirror' THEN
          CASE WHEN ls.c_camera IN ('ShortRed', 'ShortBlue') THEN 'ShortCamLongSlit'
               ELSE 'LongCamLongSlit'
          END
        WHEN COALESCE(ls.c_prism, ls.c_initial_prism) = 'Sxd' THEN 'ShortCamCrossDispersed'
        ELSE 'LongCamCrossDispersed' -- Lxd
      END)::e_gnirs_decker AS c_decker_default,
      (CASE
        WHEN ls.c_camera IN ('ShortBlue', 'LongBlue') THEN 'Shallow'
        WHEN ls.c_camera IN ('ShortRed',  'LongRed')  THEN 'Deep'
      END)::e_gnirs_well_depth AS c_well_depth_default,
      -- IFU telescope configs have no derived default (seeded at creation); only long slit.
      -- ATTENTION: The science branch is duplicated from lucuma-core
      -- alongSlitDefaultTelescopeConfigs. Modify in sync. The telluric branch mirrors
      -- it with flipped signs, except for the Order2/Order1/PAH case, which is the
      -- same pattern for both roles. This view is the only source of the telluric
      -- pattern: GnirsSpectroscopyService.applyGnirsTelluricDefaults no longer
      -- writes it, it just clears the explicit override.
      CASE
        WHEN ls.c_observing_mode_type = 'gnirs_ifu' THEN NULL
        WHEN o.c_calibration_role = 'telluric' THEN
          CASE
            WHEN COALESCE(ls.c_prism, ls.c_initial_prism) IN ('Sxd', 'Lxd') THEN
              '[{"q":{"microarcseconds":1000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-2000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-2000000},"guiding":"ENABLED"},{"q":{"microarcseconds":1000000},"guiding":"ENABLED"}]'
            WHEN ls.c_camera IN ('ShortBlue', 'ShortRed') THEN
              '[{"q":{"microarcseconds":-2000000},"guiding":"ENABLED"},{"q":{"microarcseconds":4000000},"guiding":"ENABLED"},{"q":{"microarcseconds":4000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-2000000},"guiding":"ENABLED"}]'
            WHEN ls.c_filter IN ('Order2', 'Order1', 'PAH') THEN
              '[{"q":{"microarcseconds":-3000000},"guiding":"ENABLED"},{"q":{"microarcseconds":3000000},"guiding":"ENABLED"},{"q":{"microarcseconds":3000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-3000000},"guiding":"ENABLED"}]'
            ELSE
              '[{"q":{"microarcseconds":1000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-5000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-5000000},"guiding":"ENABLED"},{"q":{"microarcseconds":1000000},"guiding":"ENABLED"}]'
          END
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

-- The default decker is part of the generated sequence, so the time estimate
-- of every LC SXD observation without an explicit decker is stale.
UPDATE t_obscalc c
   SET c_obscalc_state = 'pending'
  FROM t_gnirs_spectroscopy ls
 WHERE c.c_observation_id = ls.c_observation_id
   AND ls.c_observing_mode_type = 'gnirs_long_slit'
   AND ls.c_decker IS NULL
   AND ls.c_camera IN ('LongBlue', 'LongRed')
   AND COALESCE(ls.c_prism, ls.c_initial_prism) = 'Sxd';
