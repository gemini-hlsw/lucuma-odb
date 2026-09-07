-- A GNIRS long slit telluric's default slit offsets are the science offsets
-- flipped in sign (per GNIRS configuration), not the science offsets themselves
-- (these values used to live in GnirsSpectroscopyService.applyGnirsTelluricDefaults,
-- which wrote them into the explicit columns). Left as an explicit override, the
-- API reported a telluric as customizing a science default it never actually
-- chose, and reverting it restored the science pattern. Make the view's default
-- role-aware instead, so a telluric's own pattern is the default and nothing need
-- be stored explicitly.

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
      -- ATTENTION: The telluric branch mirrors the science one below with flipped
      -- signs, except for the Order2/Order1/PAH case, which is the same pattern for
      -- both roles. These values used to be hardcoded in
      -- GnirsSpectroscopyService.applyGnirsTelluricDefaults; keep the two in sync.
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

-- Existing long slit tellurics whose explicit offsets just duplicate the (now
-- role-aware) default were only ever set by applyGnirsTelluricDefaults, never
-- hand-edited, so null them out and let them fall back to the default like new
-- ones do. A telluric whose explicit value differs from the pattern was edited
-- by hand and is left alone.
UPDATE t_gnirs_spectroscopy ls
SET c_slit_offset_mode = NULL, c_telescope_configs = NULL
FROM v_gnirs_spectroscopy v
WHERE v.c_observation_id = ls.c_observation_id
  AND ls.c_observing_mode_type = 'gnirs_long_slit'
  AND is_telluric_calibration(ls.c_observation_id)
  AND ls.c_slit_offset_mode = v.c_slit_offset_mode_default
  AND ls.c_telescope_configs::jsonb = v.c_telescope_configs_default::jsonb;
