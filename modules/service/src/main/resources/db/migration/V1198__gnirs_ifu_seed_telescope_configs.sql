-- GNIRS IFU telescope configs are no longer a view-computed default coalesced with an
-- explicit override. They are a single stored value, seeded at observing-mode creation
-- (in the service, from lucuma-core's gnirsIfuTelescopeConfigPresets) and thereafter
-- edited in place. Long slit keeps its derived default / effective columns.

-- Backfill any existing IFU rows that relied on the (now removed) view default. Fresh
-- databases have no rows here; this only matters where V1196 already ran. The JSON is the
-- head preset per FPU (LR 'Extended', HR 'Science'), matching defaultIfuTelescopeConfigs.
UPDATE t_gnirs_spectroscopy
  SET c_telescope_configs = CASE c_fpu_ifu
    WHEN 'LowResolution'  THEN '[{"offset":{"p":{"microarcseconds":150000},"q":{"microarcseconds":150000}},"guiding":"ENABLED"},{"offset":{"p":{"microarcseconds":10000000},"q":{"microarcseconds":10000000}},"guiding":"DISABLED"},{"offset":{"p":{"microarcseconds":-150000},"q":{"microarcseconds":-150000}},"guiding":"ENABLED"},{"offset":{"p":{"microarcseconds":-10000000},"q":{"microarcseconds":-10000000}},"guiding":"DISABLED"}]'
    WHEN 'HighResolution' THEN '[{"offset":{"p":{"microarcseconds":100000},"q":{"microarcseconds":-100000}},"guiding":"ENABLED"},{"offset":{"p":{"microarcseconds":10000000},"q":{"microarcseconds":10000000}},"guiding":"DISABLED"},{"offset":{"p":{"microarcseconds":-100000},"q":{"microarcseconds":100000}},"guiding":"ENABLED"},{"offset":{"p":{"microarcseconds":-10000000},"q":{"microarcseconds":-10000000}},"guiding":"DISABLED"}]'
  END
  WHERE c_observing_mode_type = 'gnirs_ifu' AND c_telescope_configs IS NULL;

-- IFU rows now always carry telescope configs (and never a slit offset mode). Long slit
-- keeps its both-or-neither coupling of (c_telescope_configs, c_slit_offset_mode).
ALTER TABLE t_gnirs_spectroscopy
  DROP CONSTRAINT t_gnirs_spectroscopy_telescope_configs_check;
ALTER TABLE t_gnirs_spectroscopy
  ADD CONSTRAINT t_gnirs_spectroscopy_telescope_configs_check CHECK (
    CASE c_observing_mode_type
      WHEN 'gnirs_ifu' THEN c_slit_offset_mode IS NULL AND c_telescope_configs IS NOT NULL
      ELSE (c_telescope_configs IS NULL) = (c_slit_offset_mode IS NULL)
    END
  );

DROP VIEW v_gnirs_spectroscopy;
CREATE VIEW v_gnirs_spectroscopy AS
  SELECT
    ls.*,
    COALESCE(ls.c_central_wavelength, ls.c_initial_central_wavelength) AS c_central_wavelength_effective,
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
      CASE
        WHEN ls.c_observing_mode_type = 'gnirs_ifu' THEN NULL
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
