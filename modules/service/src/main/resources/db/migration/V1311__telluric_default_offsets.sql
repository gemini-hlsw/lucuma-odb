-- Telluric slit offsets are a role-aware default, not an explicit override.
--
-- The telluric resets for GNIRS long slit, Flamingos 2 MOS-derived tellurics
-- and IGRINS-2 wrote the telluric offset pattern into the explicit columns
-- while the views kept the science pattern as the default. The API therefore
-- reported every telluric as customizing a science default it never chose, and
-- reverting restored the science offsets.
--
-- For each mode: compute the telluric pattern in the view where it is not
-- already the default, then null out existing tellurics whose explicit values
-- merely duplicate that default. Those were only ever set by the reset code,
-- never hand-edited; a telluric whose explicit value differs from the pattern
-- was edited by hand and is left alone. Effective offsets are unchanged.

------------------------------------------------------------------------------
-- GNIRS long slit
------------------------------------------------------------------------------

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

-- Data fix: long slit tellurics still carrying the pattern applyGnirsTelluricDefaults wrote.
UPDATE t_gnirs_spectroscopy ls
SET c_slit_offset_mode = NULL, c_telescope_configs = NULL
FROM v_gnirs_spectroscopy v
WHERE v.c_observation_id = ls.c_observation_id
  AND ls.c_observing_mode_type = 'gnirs_long_slit'
  AND is_telluric_calibration(ls.c_observation_id)
  AND ls.c_slit_offset_mode = v.c_slit_offset_mode_default
  AND ls.c_telescope_configs::jsonb = v.c_telescope_configs_default::jsonb;

------------------------------------------------------------------------------
-- Flamingos 2 MOS
------------------------------------------------------------------------------

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

-- Data fix: MOS tellurics still carrying the pattern applyF2MosTelluricDefaults wrote.
UPDATE t_flamingos_2_long_slit m
SET c_slit_offset_mode = NULL, c_telescope_configs = NULL
FROM v_flamingos_2_long_slit v
WHERE v.c_observation_id = m.c_observation_id
  AND m.c_telluric_science_mode = 'flamingos_2_mos'
  AND m.c_slit_offset_mode = v.c_slit_offset_mode_default
  AND m.c_telescope_configs::jsonb = v.c_telescope_configs_default::jsonb;

------------------------------------------------------------------------------
-- IGRINS-2
------------------------------------------------------------------------------

-- v_igrins_2_long_slit already computes the NodAlongSlit pattern tellurics need
-- as its default, so only the data fix is required here.
UPDATE t_igrins_2_long_slit m
SET c_slit_offset_mode = NULL, c_telescope_configs = NULL
FROM v_igrins_2_long_slit v
WHERE v.c_observation_id = m.c_observation_id
  AND is_telluric_calibration(m.c_observation_id)
  AND m.c_slit_offset_mode = v.c_slit_offset_mode_default
  AND m.c_telescope_configs::jsonb = v.c_telescope_configs_default::jsonb;
