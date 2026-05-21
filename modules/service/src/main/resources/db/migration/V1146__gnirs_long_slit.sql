-- GNIRS Long Slit observing mode: lookup tables, main table, view, mode registration,
-- configuration request columns, and ETM trigger update.

-- Lookup table for GnirsDecker
CREATE TABLE t_gnirs_decker (
  c_tag        d_tag   NOT NULL PRIMARY KEY,
  c_short_name varchar NOT NULL,
  c_long_name  varchar NOT NULL
);
INSERT INTO t_gnirs_decker VALUES ('Acquisition',            'Acquisition',       'Acquisition');
INSERT INTO t_gnirs_decker VALUES ('PupilViewer',            'Pupil',             'Pupil viewer');
INSERT INTO t_gnirs_decker VALUES ('ShortCamCrossDispersed', 'Short camera XD',   'Short camera cross dispersed');
INSERT INTO t_gnirs_decker VALUES ('LongCamLongSlit',        'Long camera slit',  'Long camera long slit');
INSERT INTO t_gnirs_decker VALUES ('ShortCamLongSlit',       'Short camera slit', 'Short camera long slit');
INSERT INTO t_gnirs_decker VALUES ('LongCamCrossDispersed',  'Long camera XD',    'Long camera cross dispersed');

-- Lookup table for GnirsObsReadMode (used for both science and acquisition read mode columns).
-- 'Automatic' is the GnirsObsReadMode discriminant for per-step resolution;
-- the other four are the underlying GnirsReadMode values.
CREATE TABLE t_gnirs_obs_read_mode (
  c_tag        d_tag   NOT NULL PRIMARY KEY,
  c_short_name varchar NOT NULL,
  c_long_name  varchar NOT NULL
);
INSERT INTO t_gnirs_obs_read_mode VALUES ('Automatic',  'Automatic',   'Automatic in each step');
INSERT INTO t_gnirs_obs_read_mode VALUES ('VeryBright', 'Very bright', 'Very Bright Acquisition or High Background');
INSERT INTO t_gnirs_obs_read_mode VALUES ('Bright',     'Bright',      'Bright objects');
INSERT INTO t_gnirs_obs_read_mode VALUES ('Faint',      'Faint',       'Faint objects');
INSERT INTO t_gnirs_obs_read_mode VALUES ('VeryFaint',  'Very faint',  'Very faint objects');

CREATE TABLE t_gnirs_well_depth (
  c_tag        d_tag   NOT NULL PRIMARY KEY,
  c_short_name varchar NOT NULL,
  c_long_name  varchar NOT NULL
);
INSERT INTO t_gnirs_well_depth VALUES ('Shallow', 'Shallow', 'Shallow well');
INSERT INTO t_gnirs_well_depth VALUES ('Deep',    'Deep',    'Deep well');

-- Main GNIRS long slit table
CREATE TABLE t_gnirs_long_slit (
  c_observation_id      d_observation_id      NOT NULL PRIMARY KEY,
  c_program_id          d_program_id          NOT NULL,
  c_observing_mode_type e_observing_mode_type NOT NULL DEFAULT 'gnirs_long_slit',
  CHECK (c_observing_mode_type = 'gnirs_long_slit'),

  c_grating            d_tag           NULL REFERENCES t_gnirs_grating(c_tag),
  c_prism              d_tag           NULL REFERENCES t_gnirs_prism(c_tag),
  c_grating_wavelength d_wavelength_pm NULL,

  -- Initial grating/prism for configuration grouping key
  c_initial_grating d_tag NOT NULL REFERENCES t_gnirs_grating(c_tag),
  c_initial_prism   d_tag NOT NULL REFERENCES t_gnirs_prism(c_tag),

  -- Camera: standalone single value; initial tracked separately
  c_camera         d_tag NOT NULL REFERENCES t_gnirs_camera(c_tag),
  c_initial_camera d_tag NOT NULL REFERENCES t_gnirs_camera(c_tag),

  c_fpu                        d_tag           NOT NULL REFERENCES t_gnirs_fpu_slit(c_tag),
  c_central_wavelength         d_wavelength_pm NOT NULL,
  c_initial_fpu                d_tag           NOT NULL REFERENCES t_gnirs_fpu_slit(c_tag),
  c_initial_central_wavelength d_wavelength_pm NOT NULL,

  -- filter: single required value; initial tracked separately
  c_filter         d_tag NOT NULL REFERENCES t_gnirs_filter(c_tag),
  c_initial_filter d_tag NOT NULL REFERENCES t_gnirs_filter(c_tag),

  -- Science coadds
  c_coadds int4 NOT NULL DEFAULT 1 CHECK (c_coadds > 0),

  -- Explicit overrides (NULL = use computed default)
  c_decker            d_tag    NULL REFERENCES t_gnirs_decker(c_tag),
  c_focus_motor_steps integer  NULL CHECK (c_focus_motor_steps BETWEEN -179999 AND 180000),
  c_read_mode         d_tag    NULL REFERENCES t_gnirs_obs_read_mode(c_tag),
  c_well_depth        d_tag    NULL REFERENCES t_gnirs_well_depth(c_tag),

  -- SlitTelescopeConfigs: discriminant + serialized JSON config list (NULL = use computed default)
  c_slit_offset_mode d_tag NULL REFERENCES t_slit_offset_mode(c_tag),
  c_telescope_configs text  NULL,

  -- Acquisition configuration (TimeAndCount ETM stored inline)
  c_acq_read_mode d_tag           NOT NULL REFERENCES t_gnirs_obs_read_mode(c_tag),
  c_acq_coadds    int4            NOT NULL DEFAULT 1 CHECK (c_acq_coadds > 0),
  c_acq_filter    d_tag           NOT NULL REFERENCES t_gnirs_filter(c_tag),
  c_acq_offset_p  d_angle_µas NULL,
  c_acq_offset_q  d_angle_µas NULL,
  c_acq_exp_time  interval        NOT NULL CHECK (c_acq_exp_time >= '0'),
  c_acq_exp_count int4            NOT NULL CHECK (c_acq_exp_count > 0),
  c_acq_exp_at    d_wavelength_pm NOT NULL,

  -- Mode key for config grouping (initial grating/prism/fpu)
  c_mode_key text GENERATED ALWAYS AS (
    c_initial_grating || '/' || c_initial_prism || '/' || c_fpu
  ) STORED,

  FOREIGN KEY (c_observation_id, c_observing_mode_type)
    REFERENCES t_observation(c_observation_id, c_observing_mode_type)
    ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED
);

-- DB view with derived defaults and effective values.
-- Uses CROSS JOIN LATERAL so grating_wavelength_default and telescope_configs_default
-- are computed once and referenced in both the default and effective columns.
-- c_*_default  = pure computed default (no COALESCE with explicit)
-- c_*_effective = COALESCE(explicit, default)
CREATE VIEW v_gnirs_long_slit AS
  SELECT
    ls.*,
    -- decker pure default: mirrors GnirsDecker.forCameraAndReadMode(camera, effectivePrism)
    -- ATTENTION: This logic is duplicated from lucuma-core GnirsDecker. Modify in sync.
    CASE
      WHEN COALESCE(ls.c_prism, ls.c_initial_prism) = 'Mirror' THEN
        CASE WHEN ls.c_camera IN ('ShortRed', 'ShortBlue') THEN 'ShortCamLongSlit'::varchar
             ELSE 'LongCamLongSlit'::varchar
        END
      ELSE -- Sxd or Lxd
        CASE WHEN ls.c_camera IN ('ShortRed', 'ShortBlue') THEN 'ShortCamCrossDispersed'::varchar
             ELSE 'LongCamCrossDispersed'::varchar
        END
    END AS c_decker_default,
    -- grating wavelength default (computed once in lateral, referenced here and in effective)
    d.c_grating_wavelength_default,
    COALESCE(ls.c_grating_wavelength, d.c_grating_wavelength_default) AS c_grating_wavelength_effective,
    -- read mode default: Automatic (resolved per-step at sequence generation time)
    'Automatic'::varchar AS c_read_mode_default,
    -- well depth pure default: mirrors GnirsWellDepth.forCamera
    CASE
      WHEN ls.c_camera IN ('ShortBlue', 'LongBlue') THEN 'Shallow'::varchar
      WHEN ls.c_camera IN ('ShortRed',  'LongRed')  THEN 'Deep'::varchar
    END AS c_well_depth_default,
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

-- Extend v_all_modes with GNIRS LongSlit and refresh v_observing_mode_group (same
-- pattern as V1119__ghost.sql).
CREATE OR REPLACE VIEW v_all_modes AS
  SELECT c_mode_key, c_observation_id FROM t_flamingos_2_long_slit
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_ghost_ifu
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_gmos_north_long_slit
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_gmos_south_long_slit
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_igrins_2_long_slit
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_gnirs_long_slit;

CREATE OR REPLACE VIEW v_observing_mode_group AS
  SELECT
    m.c_mode_key,
    o.c_program_id,
    max(m.c_observation_id) as c_observation_id
  FROM
    v_all_modes m
  JOIN t_observation o USING (c_observation_id)
  GROUP BY
    m.c_mode_key,
    o.c_program_id;

-- Register mode for observing mode consistency trigger
SELECT register_observing_mode('gnirs_long_slit', 't_gnirs_long_slit');

-- Update check_etm_consistent to handle gnirs_long_slit:
-- acquisition ETM is stored inline in t_gnirs_long_slit; only one science ETM in t_exposure_time_mode
CREATE OR REPLACE FUNCTION check_etm_consistent()
RETURNS TRIGGER AS $$
DECLARE
  obs_id   d_observation_id;
  obs_mode e_observing_mode_type;
  acq_count INTEGER;
  sci_count INTEGER;
BEGIN

  obs_id := COALESCE(NEW.c_observation_id, OLD.c_observation_id);

  SELECT c_observing_mode_type INTO obs_mode
    FROM t_observation
   WHERE c_observation_id = obs_id;

  SELECT
    COUNT(*) FILTER (WHERE c_role = 'acquisition'),
    COUNT(*) FILTER (WHERE c_role = 'science')
  INTO acq_count, sci_count
  FROM t_exposure_time_mode
  WHERE c_observation_id = obs_id;

  IF obs_mode IS NULL THEN

    IF acq_count <> 0 OR sci_count <> 0 THEN
      RAISE EXCEPTION 'Observation % with mode % should not have acquisition nor science exposure time modes', obs_id, obs_mode;
    END IF;

  ELSE

    CASE
      WHEN obs_mode IN ('flamingos_2_long_slit', 'gmos_north_long_slit', 'gmos_south_long_slit') THEN
        IF acq_count <> 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have an acquisition exposure time mode', obs_id, obs_mode;
        END IF;

        IF sci_count <> 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have exactly one science exposure time mode', obs_id, obs_mode;
        END IF;

      WHEN obs_mode = 'ghost_ifu' THEN
        IF sci_count <> 2 THEN
          RAISE EXCEPTION 'Observation % with mode % must have two science exposure time modes (red and blue camera)', obs_id, obs_mode;
        END IF;

      WHEN obs_mode IN ('igrins_2_long_slit', 'gnirs_long_slit') THEN
        IF sci_count <> 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have exactly one science exposure time mode', obs_id, obs_mode;
        END IF;

      WHEN obs_mode IN ('gmos_north_imaging', 'gmos_south_imaging') THEN
        NULL;

      -- no checks for visitor modes
      WHEN obs_mode IN (
        'alopeke_speckle',
        'alopeke_wide_field',
        'visitor_north',
        'visitor_south',
        'zorro_speckle',
        'zorro_wide_field',
        'maroon_x'
      ) THEN
        NULL;

      ELSE
        RAISE EXCEPTION 'Unknown observing mode % for observation %', obs_mode, obs_id;
    END CASE;
  END IF;

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

-- Add GNIRS columns to t_configuration_request.
-- Configuration.ObservingMode.GnirsLongSlit is keyed by (grating, camera, prism).
DROP VIEW v_configuration_request;

ALTER TABLE t_configuration_request
  ADD COLUMN c_gnirs_longslit_grating d_tag NULL REFERENCES t_gnirs_grating(c_tag),
  ADD COLUMN c_gnirs_longslit_camera  d_tag NULL REFERENCES t_gnirs_camera(c_tag),
  ADD COLUMN c_gnirs_longslit_prism   d_tag NULL REFERENCES t_gnirs_prism(c_tag);

CREATE VIEW v_configuration_request AS
  SELECT
    *,
    CASE WHEN cr.c_reference_ra IS NOT NULL THEN cr.c_configuration_request_id END AS c_reference_id,
    CASE WHEN cr.c_region_ra_arc_type IS NOT NULL THEN cr.c_configuration_request_id END AS c_region_id,
    CASE WHEN cr.c_observing_mode_type = 'flamingos_2_long_slit' THEN cr.c_configuration_request_id END AS c_flamingos_2_longslit_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_north_imaging' THEN cr.c_configuration_request_id END AS c_gmos_north_imaging_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_south_imaging' THEN cr.c_configuration_request_id END AS c_gmos_south_imaging_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_north_long_slit' THEN cr.c_configuration_request_id END AS c_gmos_north_longslit_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_south_long_slit' THEN cr.c_configuration_request_id END AS c_gmos_south_longslit_id,
    CASE WHEN cr.c_observing_mode_type = 'igrins_2_long_slit' THEN cr.c_configuration_request_id END AS c_igrins_2_longslit_id,
    CASE WHEN cr.c_gnirs_longslit_grating IS NOT NULL AND cr.c_gnirs_longslit_camera IS NOT NULL AND cr.c_gnirs_longslit_prism IS NOT NULL THEN cr.c_configuration_request_id END AS c_gnirs_longslit_id,
    CASE WHEN cr.c_visitor_radius IS NOT NULL THEN cr.c_configuration_request_id END AS c_visitor_id,
    CASE WHEN cr.c_visitor_radius IS NOT NULL THEN cr.c_observing_mode_type END AS c_visitor_mode,
    CASE WHEN cr.c_region_ra_arc_type = 'partial' THEN cr.c_configuration_request_id END AS c_partial_ra_region_id,
    CASE WHEN cr.c_region_dec_arc_type = 'partial' THEN cr.c_configuration_request_id END AS c_partial_dec_region_id
  FROM t_configuration_request cr
  ;
