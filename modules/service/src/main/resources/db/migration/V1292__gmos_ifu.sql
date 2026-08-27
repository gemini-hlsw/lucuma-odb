-- GMOS North/South IFU observing mode.
--
-- The mode is GMOS long slit with the slit replaced by one of the two IFU
-- apertures, so it reuses the long slit grouping key, the long slit science
-- sequence and the long slit acquisition filter default.  What differs is that
-- the IFU samples a field rather than a slit: it reads out unbinned, and it
-- carries the sampling geometry the ITC integrates over.

-- The apertures the mode offers: both pseudo-slits, or either one alone.  The
-- South also carries nod & shuffle units, which are not offered because nothing
-- generates a nod & shuffle sequence yet.  The values are the same at both
-- sites, so one table backs both.
CREATE TABLE t_gmos_ifu_fpu (
  c_tag         d_tag  PRIMARY KEY,
  c_short_name  text   NOT NULL,
  c_long_name   text   NOT NULL,
  c_field_width d_angle_µas NOT NULL
);

COMMENT ON TABLE  t_gmos_ifu_fpu IS 'GMOS IFU focal plane units offered by the IFU observing mode';
COMMENT ON COLUMN t_gmos_ifu_fpu.c_field_width IS 'Width of the target lenslet field across p; masking to one pseudo-slit halves it.';

INSERT INTO t_gmos_ifu_fpu VALUES ('TwoSlits',    'IFU-2', 'IFU 2 Slits',           7000000);
INSERT INTO t_gmos_ifu_fpu VALUES ('OneSlitRed',  'IFU-R', 'IFU Right Slit (red)',  3500000);
INSERT INTO t_gmos_ifu_fpu VALUES ('OneSlitBlue', 'IFU-B', 'IFU Left Slit (blue)',  3500000);

CREATE TABLE t_gmos_north_ifu (

  c_observation_id             d_observation_id      NOT NULL,
  c_program_id                 d_program_id          NOT NULL,
  c_instrument                 d_tag                 NOT NULL DEFAULT 'GmosNorth' REFERENCES t_instrument(c_tag) CHECK (c_instrument = 'GmosNorth'),
  c_observing_mode_type        e_observing_mode_type NOT NULL DEFAULT 'gmos_north_ifu' CHECK (c_observing_mode_type = 'gmos_north_ifu'),

  c_grating                    d_tag                 NOT NULL          REFERENCES t_gmos_north_disperser(c_tag),
  c_filter                     d_tag                 NULL DEFAULT NULL REFERENCES t_gmos_north_filter(c_tag),
  c_fpu                        d_tag                 NOT NULL          REFERENCES t_gmos_ifu_fpu(c_tag),
  c_central_wavelength         d_wavelength_pm       NOT NULL,

  -- Explicit overrides
  c_xbin                       d_tag                 NULL DEFAULT NULL REFERENCES t_gmos_binning(c_tag),
  c_xbin_default               d_tag                 NOT NULL DEFAULT 'One'::d_tag REFERENCES t_gmos_binning(c_tag),
  c_ybin                       d_tag                 NULL DEFAULT NULL REFERENCES t_gmos_binning(c_tag),
  c_ybin_default               d_tag                 NOT NULL DEFAULT 'One'::d_tag REFERENCES t_gmos_binning(c_tag),
  c_amp_read_mode              d_tag                 NULL DEFAULT NULL REFERENCES t_gmos_amp_read_mode(c_tag),
  c_amp_gain                   d_tag                 NULL DEFAULT NULL REFERENCES t_gmos_amp_gain(c_tag),
  c_roi                        d_tag                 NULL DEFAULT NULL REFERENCES t_gmos_roi(c_tag),

  c_wavelength_dithers         text                  NULL DEFAULT NULL,

  -- The IFU dithers within its field rather than nodding along a slit, so it
  -- stores a plain list of telescope configurations, as MOS does, with no slit
  -- offset mode to pair with it.
  c_telescope_configs          text                  NULL DEFAULT NULL,

  -- How the ITC samples the field: sum every element within a radius of the
  -- centre, or measure the single element at an offset.  At most one is set;
  -- neither means the default sampling applies.
  c_ifu_analysis_sum_radius    d_angle_µas           NULL DEFAULT NULL,
  c_ifu_analysis_single_offset d_angle_µas           NULL DEFAULT NULL,

  c_acquisition_filter         d_tag                 NULL DEFAULT NULL REFERENCES t_gmos_north_filter(c_tag),

  c_initial_grating            d_tag                 NOT NULL          REFERENCES t_gmos_north_disperser(c_tag),
  c_initial_filter             d_tag                 NULL DEFAULT NULL REFERENCES t_gmos_north_filter(c_tag),
  c_initial_fpu                d_tag                 NOT NULL          REFERENCES t_gmos_ifu_fpu(c_tag),
  c_initial_central_wavelength d_wavelength_pm       NOT NULL,

  CONSTRAINT gmos_north_ifu_wavelength_dither_format CHECK (c_wavelength_dithers ~ '^-?\d+(\.\d+)?(,-?\d+(\.\d+)?)*$'),

  CONSTRAINT gmos_north_ifu_analysis_check
    CHECK (num_nonnulls(c_ifu_analysis_sum_radius, c_ifu_analysis_single_offset) <= 1),

  -- A sum radius encloses lenslets, so it must be positive; a single offset may
  -- sit either side of the field centre.
  CONSTRAINT gmos_north_ifu_analysis_sum_radius_check
    CHECK (c_ifu_analysis_sum_radius IS NULL OR c_ifu_analysis_sum_radius > 0),

  PRIMARY KEY (c_observation_id, c_instrument, c_observing_mode_type),
  UNIQUE (c_observation_id),
  FOREIGN KEY (c_observation_id, c_instrument, c_observing_mode_type)
    REFERENCES t_observation(c_observation_id, c_instrument, c_observing_mode_type)
    ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED
);

COMMENT ON TABLE t_gmos_north_ifu IS 'GMOS North IFU mode configuration';

CREATE TABLE t_gmos_south_ifu (

  c_observation_id             d_observation_id      NOT NULL,
  c_program_id                 d_program_id          NOT NULL,
  c_instrument                 d_tag                 NOT NULL DEFAULT 'GmosSouth' REFERENCES t_instrument(c_tag) CHECK (c_instrument = 'GmosSouth'),
  c_observing_mode_type        e_observing_mode_type NOT NULL DEFAULT 'gmos_south_ifu' CHECK (c_observing_mode_type = 'gmos_south_ifu'),

  c_grating                    d_tag                 NOT NULL          REFERENCES t_gmos_south_disperser(c_tag),
  c_filter                     d_tag                 NULL DEFAULT NULL REFERENCES t_gmos_south_filter(c_tag),
  c_fpu                        d_tag                 NOT NULL          REFERENCES t_gmos_ifu_fpu(c_tag),
  c_central_wavelength         d_wavelength_pm       NOT NULL,

  c_xbin                       d_tag                 NULL DEFAULT NULL REFERENCES t_gmos_binning(c_tag),
  c_xbin_default               d_tag                 NOT NULL DEFAULT 'One'::d_tag REFERENCES t_gmos_binning(c_tag),
  c_ybin                       d_tag                 NULL DEFAULT NULL REFERENCES t_gmos_binning(c_tag),
  c_ybin_default               d_tag                 NOT NULL DEFAULT 'One'::d_tag REFERENCES t_gmos_binning(c_tag),
  c_amp_read_mode              d_tag                 NULL DEFAULT NULL REFERENCES t_gmos_amp_read_mode(c_tag),
  c_amp_gain                   d_tag                 NULL DEFAULT NULL REFERENCES t_gmos_amp_gain(c_tag),
  c_roi                        d_tag                 NULL DEFAULT NULL REFERENCES t_gmos_roi(c_tag),

  c_wavelength_dithers         text                  NULL DEFAULT NULL,

  -- The IFU dithers within its field rather than nodding along a slit, so it
  -- stores a plain list of telescope configurations, as MOS does, with no slit
  -- offset mode to pair with it.
  c_telescope_configs          text                  NULL DEFAULT NULL,

  c_ifu_analysis_sum_radius    d_angle_µas           NULL DEFAULT NULL,
  c_ifu_analysis_single_offset d_angle_µas           NULL DEFAULT NULL,

  c_acquisition_filter         d_tag                 NULL DEFAULT NULL REFERENCES t_gmos_south_filter(c_tag),

  c_initial_grating            d_tag                 NOT NULL          REFERENCES t_gmos_south_disperser(c_tag),
  c_initial_filter             d_tag                 NULL DEFAULT NULL REFERENCES t_gmos_south_filter(c_tag),
  c_initial_fpu                d_tag                 NOT NULL          REFERENCES t_gmos_ifu_fpu(c_tag),
  c_initial_central_wavelength d_wavelength_pm       NOT NULL,

  CONSTRAINT gmos_south_ifu_wavelength_dither_format CHECK (c_wavelength_dithers ~ '^-?\d+(\.\d+)?(,-?\d+(\.\d+)?)*$'),

  CONSTRAINT gmos_south_ifu_analysis_check
    CHECK (num_nonnulls(c_ifu_analysis_sum_radius, c_ifu_analysis_single_offset) <= 1),

  CONSTRAINT gmos_south_ifu_analysis_sum_radius_check
    CHECK (c_ifu_analysis_sum_radius IS NULL OR c_ifu_analysis_sum_radius > 0),

  PRIMARY KEY (c_observation_id, c_instrument, c_observing_mode_type),
  UNIQUE (c_observation_id),
  FOREIGN KEY (c_observation_id, c_instrument, c_observing_mode_type)
    REFERENCES t_observation(c_observation_id, c_instrument, c_observing_mode_type)
    ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED
);

COMMENT ON TABLE t_gmos_south_ifu IS 'GMOS South IFU mode configuration';

-- Binning.  The IFU always reads out unbinned to preserve spatial sampling, so
-- there is deliberately no binning trigger here: the c_*_default columns are
-- already 'One' and nothing recomputes them.  `set_gmos_default_binning` falls
-- through its ELSE for these modes.

-- Mode grouping.  The IFU keys on the same fields as long slit, with the IFU
-- aperture in the FPU slot.  The ITC sampling geometry is deliberately absent:
-- two IFU observations differing only in how the ITC integrates the field take
-- identical calibrations.
ALTER TABLE t_gmos_north_ifu
  ADD COLUMN c_mode_key text NOT NULL GENERATED ALWAYS AS (
    format_gmos_long_slit_mode_group(
      'gn',
      c_program_id,
      c_observing_mode_type,
      c_grating,
      c_filter,
      c_fpu,
      c_central_wavelength,
      c_xbin,
      c_xbin_default,
      c_ybin,
      c_ybin_default,
      c_amp_read_mode,
      c_amp_gain,
      c_roi,
      c_wavelength_dithers,
      -- The effective value is keyed, so spelling out the default groups with
      -- falling through to it.
      -- ATTENTION: duplicated from gmos.ifu.Config.DefaultTelescopeConfigs
      -- (transport codec).  Keep in sync.
      COALESCE(c_telescope_configs, '[{"offset":{"p":{"microarcseconds":0},"q":{"microarcseconds":0}},"guiding":"ENABLED"}]')
    )
  ) STORED;

ALTER TABLE t_gmos_south_ifu
  ADD COLUMN c_mode_key text NOT NULL GENERATED ALWAYS AS (
    format_gmos_long_slit_mode_group(
      'gs',
      c_program_id,
      c_observing_mode_type,
      c_grating,
      c_filter,
      c_fpu,
      c_central_wavelength,
      c_xbin,
      c_xbin_default,
      c_ybin,
      c_ybin_default,
      c_amp_read_mode,
      c_amp_gain,
      c_roi,
      c_wavelength_dithers,
      -- The effective value is keyed, so spelling out the default groups with
      -- falling through to it.
      -- ATTENTION: duplicated from gmos.ifu.Config.DefaultTelescopeConfigs
      -- (transport codec).  Keep in sync.
      COALESCE(c_telescope_configs, '[{"offset":{"p":{"microarcseconds":0},"q":{"microarcseconds":0}},"guiding":"ENABLED"}]')
    )
  ) STORED;

CREATE TRIGGER observing_mode_key_trigger
AFTER INSERT OR DELETE OR UPDATE OF c_mode_key ON t_gmos_north_ifu
FOR EACH ROW
EXECUTE FUNCTION trigger_set_observation_mode_key();

CREATE TRIGGER observing_mode_key_trigger
AFTER INSERT OR DELETE OR UPDATE OF c_mode_key ON t_gmos_south_ifu
FOR EACH ROW
EXECUTE FUNCTION trigger_set_observation_mode_key();

DROP VIEW v_observing_mode_group;
DROP VIEW v_all_modes;

CREATE VIEW v_all_modes AS
  SELECT c_mode_key, c_observation_id FROM t_flamingos_2_long_slit
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_flamingos_2_mos
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_ghost_ifu
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_gmos_north_long_slit
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_gmos_north_mos
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_gmos_north_ifu
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_gmos_south_long_slit
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_gmos_south_mos
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_gmos_south_ifu
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_igrins_2_long_slit
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_gnirs_spectroscopy;

CREATE VIEW v_observing_mode_group AS
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

-- The mode views, exposing the wavelength-nearest default acquisition filter
-- exactly as long slit and MOS compute it.
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

-- An explicit acquisition filter must be one of the acquisition filters.
CREATE TRIGGER check_gmos_north_ifu_acquisition_filter_is_valid_trigger
BEFORE INSERT OR UPDATE OF c_acquisition_filter
ON t_gmos_north_ifu
FOR EACH ROW
EXECUTE FUNCTION check_gmos_acquisition_filter_is_valid('t_gmos_north_filter');

CREATE TRIGGER check_gmos_south_ifu_acquisition_filter_is_valid_trigger
BEFORE INSERT OR UPDATE OF c_acquisition_filter
ON t_gmos_south_ifu
FOR EACH ROW
EXECUTE FUNCTION check_gmos_acquisition_filter_is_valid('t_gmos_south_filter');

-- Register the modes for the observing mode consistency trigger.
SELECT register_observing_mode('gmos_north_ifu', 't_gmos_north_ifu');
SELECT register_observing_mode('gmos_south_ifu', 't_gmos_south_ifu');

-- Obs events and obs calc triggers
CREATE TRIGGER ch_observation_edit_gmos_north_ifu_trigger
AFTER INSERT OR UPDATE OR DELETE ON t_gmos_north_ifu
FOR EACH ROW
EXECUTE FUNCTION ch_observation_edit_associated_table_update();

CREATE TRIGGER ch_observation_edit_gmos_south_ifu_trigger
AFTER INSERT OR UPDATE OR DELETE ON t_gmos_south_ifu
FOR EACH ROW
EXECUTE FUNCTION ch_observation_edit_associated_table_update();

-- The IFU has both an acquisition and a science exposure time mode, exactly as
-- long slit does, so it joins that branch.
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
      WHEN obs_mode IN ('flamingos_2_long_slit', 'gmos_north_long_slit', 'gmos_south_long_slit', 'gmos_north_ifu', 'gmos_south_ifu') THEN
        IF acq_count <> 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have an acquisition exposure time mode', obs_id, obs_mode;
        END IF;

        IF sci_count <> 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have exactly one science exposure time mode', obs_id, obs_mode;
        END IF;

      -- GNIRS spectroscopy keeps its single acquisition ETM, but science ETMs are
      -- now per central wavelength, in both the 'initial' and 'current' row
      -- versions, so their count is not fixed.
      WHEN obs_mode IN ('gnirs_long_slit', 'gnirs_ifu') THEN
        IF acq_count <> 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have an acquisition exposure time mode', obs_id, obs_mode;
        END IF;

        IF sci_count < 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have at least one science exposure time mode', obs_id, obs_mode;
        END IF;

      WHEN obs_mode IN ('flamingos_2_mos', 'gmos_north_mos', 'gmos_south_mos') THEN
        IF acq_count <> 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have an acquisition exposure time mode', obs_id, obs_mode;
        END IF;

        IF sci_count <> 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have exactly one science exposure time mode', obs_id, obs_mode;
        END IF;

        -- MOS acquisition is sized by the observer, not solved from a
        -- signal-to-noise goal.
        IF NOT EXISTS (
          SELECT 1
            FROM t_exposure_time_mode e
           WHERE e.c_observation_id = obs_id
             AND e.c_role = 'acquisition'
             AND e.c_exposure_time_mode = 'time_and_count'
        ) THEN
          RAISE EXCEPTION 'Observation % with mode % must have a Time & Count acquisition exposure time mode', obs_id, obs_mode;
        END IF;

      WHEN obs_mode = 'ghost_ifu' THEN
        IF sci_count <> 2 THEN
          RAISE EXCEPTION 'Observation % with mode % must have two science exposure time modes (red and blue camera)', obs_id, obs_mode;
        END IF;

      WHEN obs_mode = 'igrins_2_long_slit' THEN
        IF sci_count <> 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have exactly one science exposure time mode', obs_id, obs_mode;
        END IF;

      WHEN obs_mode = 'gnirs_imaging' THEN
        IF acq_count <> 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have an acquisition exposure time mode', obs_id, obs_mode;
        END IF;

      WHEN obs_mode IN ('gmos_north_imaging', 'gmos_south_imaging', 'flamingos_2_imaging') THEN
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

      WHEN obs_mode IN ('exchange_keck', 'exchange_subaru') THEN
        IF acq_count <> 0 OR sci_count <> 0 THEN
          RAISE EXCEPTION 'Observation % with mode % should not have acquisition nor science exposure time modes', obs_id, obs_mode;
        END IF;

      ELSE
        RAISE EXCEPTION 'Unknown observing mode % for observation %', obs_mode, obs_id;
    END CASE;
  END IF;

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

-- Configuration requests.  An IFU request is discriminated by its grating and
-- aperture, mirroring Configuration.ObservingMode.Gmos{North,South}Ifu.  Unlike
-- MOS it does not reuse the long slit grating column: the aperture is part of
-- the configuration, so the pair has to travel together.
ALTER TABLE t_configuration_request
  ADD COLUMN c_gmos_north_ifu_grating d_tag NULL REFERENCES t_gmos_north_disperser(c_tag),
  ADD COLUMN c_gmos_north_ifu_fpu     d_tag NULL REFERENCES t_gmos_ifu_fpu(c_tag),
  ADD COLUMN c_gmos_south_ifu_grating d_tag NULL REFERENCES t_gmos_south_disperser(c_tag),
  ADD COLUMN c_gmos_south_ifu_fpu     d_tag NULL REFERENCES t_gmos_ifu_fpu(c_tag);

ALTER TABLE t_configuration_request
  ADD CONSTRAINT t_configuration_request_c_gmos_north_ifu_check
    CHECK ((c_gmos_north_ifu_grating IS NULL) = (c_gmos_north_ifu_fpu IS NULL)),
  ADD CONSTRAINT t_configuration_request_c_gmos_south_ifu_check
    CHECK ((c_gmos_south_ifu_grating IS NULL) = (c_gmos_south_ifu_fpu IS NULL));

DROP VIEW v_configuration_request;

CREATE VIEW v_configuration_request AS
  SELECT
    *,
    CASE WHEN cr.c_reference_ra IS NOT NULL THEN cr.c_configuration_request_id END AS c_reference_id,
    CASE WHEN cr.c_region_ra_arc_type IS NOT NULL THEN cr.c_configuration_request_id END AS c_region_id,
    CASE WHEN cr.c_observing_mode_type = 'flamingos_2_long_slit' THEN cr.c_configuration_request_id END AS c_flamingos_2_longslit_id,
    CASE WHEN cr.c_observing_mode_type = 'flamingos_2_mos' THEN cr.c_configuration_request_id END AS c_flamingos_2_mos_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_north_imaging' THEN cr.c_configuration_request_id END AS c_gmos_north_imaging_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_south_imaging' THEN cr.c_configuration_request_id END AS c_gmos_south_imaging_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_north_long_slit' THEN cr.c_configuration_request_id END AS c_gmos_north_longslit_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_south_long_slit' THEN cr.c_configuration_request_id END AS c_gmos_south_longslit_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_north_mos' THEN cr.c_configuration_request_id END AS c_gmos_north_mos_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_south_mos' THEN cr.c_configuration_request_id END AS c_gmos_south_mos_id,
    CASE WHEN cr.c_observing_mode_type = 'igrins_2_long_slit' THEN cr.c_configuration_request_id END AS c_igrins_2_longslit_id,
    CASE WHEN cr.c_gmos_north_ifu_grating IS NOT NULL AND cr.c_gmos_north_ifu_fpu IS NOT NULL THEN cr.c_configuration_request_id END AS c_gmos_north_ifu_id,
    CASE WHEN cr.c_gmos_south_ifu_grating IS NOT NULL AND cr.c_gmos_south_ifu_fpu IS NOT NULL THEN cr.c_configuration_request_id END AS c_gmos_south_ifu_id,
    CASE WHEN cr.c_gnirs_longslit_grating IS NOT NULL AND cr.c_gnirs_longslit_camera IS NOT NULL AND cr.c_gnirs_longslit_prism IS NOT NULL THEN cr.c_configuration_request_id END AS c_gnirs_longslit_id,
    CASE WHEN cr.c_gnirs_ifu_grating IS NOT NULL AND cr.c_gnirs_ifu_fpu IS NOT NULL THEN cr.c_configuration_request_id END AS c_gnirs_ifu_id,
    CASE WHEN cr.c_visitor_radius IS NOT NULL THEN cr.c_configuration_request_id END AS c_visitor_id,
    CASE WHEN cr.c_visitor_radius IS NOT NULL THEN cr.c_observing_mode_type END AS c_visitor_mode,
    CASE WHEN cr.c_region_ra_arc_type = 'partial' THEN cr.c_configuration_request_id END AS c_partial_ra_region_id,
    CASE WHEN cr.c_region_dec_arc_type = 'partial' THEN cr.c_configuration_request_id END AS c_partial_dec_region_id
  FROM t_configuration_request cr
  ;
