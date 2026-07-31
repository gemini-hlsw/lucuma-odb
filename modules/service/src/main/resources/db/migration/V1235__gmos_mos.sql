-- GMOS North/South MOS observing mode.
--
-- The mode is GMOS long slit with the builtin FPU replaced by a custom mask: a
-- required slit width plus an optional attachment.  The attachment is nullable
-- because the mask is usually only machined during Phase 2 (cf. V1230, which
-- made the same allowance for the dynamic config).
--
-- There is no acquisition configuration.  Sequence generation for MOS is not
-- implemented, and inventing an acquisition contract ahead of it would be
-- guesswork; it can be added later without disturbing what is here.

CREATE TABLE t_gmos_north_mos (

  c_observation_id             d_observation_id      NOT NULL,
  c_program_id                 d_program_id          NOT NULL,
  c_instrument                 d_tag                 NOT NULL DEFAULT 'GmosNorth' REFERENCES t_instrument(c_tag) CHECK (c_instrument = 'GmosNorth'),
  c_observing_mode_type        e_observing_mode_type NOT NULL DEFAULT 'gmos_north_mos' CHECK (c_observing_mode_type = 'gmos_north_mos'),

  c_grating                    d_tag                 NOT NULL          REFERENCES t_gmos_north_disperser(c_tag),
  c_filter                     d_tag                 NULL DEFAULT NULL REFERENCES t_gmos_north_filter(c_tag),
  c_central_wavelength         d_wavelength_pm       NOT NULL,

  -- The custom mask: slit width is always known, the machined mask often is not.
  c_slit_width                 d_tag                 NOT NULL          REFERENCES t_gmos_custom_slit_width(c_tag),
  c_mask_attachment_id         d_attachment_id       NULL DEFAULT NULL,
  c_mask_attachment_type       e_attachment_type     NULL DEFAULT NULL CHECK (c_mask_attachment_type = 'mos_mask'),

  -- Explicit overrides
  c_xbin                       d_tag                 NULL DEFAULT NULL REFERENCES t_gmos_binning(c_tag),
  c_xbin_default               d_tag                 NOT NULL DEFAULT 'One'::d_tag REFERENCES t_gmos_binning(c_tag),
  c_ybin                       d_tag                 NULL DEFAULT NULL REFERENCES t_gmos_binning(c_tag),
  c_ybin_default               d_tag                 NOT NULL DEFAULT 'One'::d_tag REFERENCES t_gmos_binning(c_tag),
  c_amp_read_mode              d_tag                 NULL DEFAULT NULL REFERENCES t_gmos_amp_read_mode(c_tag),
  c_amp_gain                   d_tag                 NULL DEFAULT NULL REFERENCES t_gmos_amp_gain(c_tag),
  c_roi                        d_tag                 NULL DEFAULT NULL REFERENCES t_gmos_roi(c_tag),

  c_wavelength_dithers         text                  NULL DEFAULT NULL,
  c_offsets                    text                  NULL DEFAULT NULL,

  c_initial_grating            d_tag                 NOT NULL          REFERENCES t_gmos_north_disperser(c_tag),
  c_initial_filter             d_tag                 NULL DEFAULT NULL REFERENCES t_gmos_north_filter(c_tag),
  c_initial_slit_width         d_tag                 NOT NULL          REFERENCES t_gmos_custom_slit_width(c_tag),
  c_initial_central_wavelength d_wavelength_pm       NOT NULL,

  CONSTRAINT gmos_north_mos_wavelength_dither_format CHECK (c_wavelength_dithers ~ '^-?\d+(\.\d+)?(,-?\d+(\.\d+)?)*$'),
  CONSTRAINT gmos_north_mos_offset_format            CHECK (c_offsets ~ '^-?\d+(\.\d+)?(,-?\d+(\.\d+)?)*$'),

  -- The type column exists only to pin the attachment's type through the
  -- composite foreign key, so it is present exactly when the id is.
  CONSTRAINT gmos_north_mos_mask_attachment_check
    CHECK ((c_mask_attachment_id IS NULL) = (c_mask_attachment_type IS NULL)),

  PRIMARY KEY (c_observation_id, c_instrument, c_observing_mode_type),
  UNIQUE (c_observation_id),
  FOREIGN KEY (c_observation_id, c_instrument, c_observing_mode_type)
    REFERENCES t_observation(c_observation_id, c_instrument, c_observing_mode_type)
    ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED,

  -- Matching on the program id as well makes another program's attachment
  -- unreferenceable, and on the type makes a non-mask attachment unreferenceable.
  -- Deleting the attachment returns the observation to "mask not yet defined";
  -- the column list keeps the program id out of it.
  CONSTRAINT gmos_north_mos_mask_attachment_fkey
    FOREIGN KEY (c_program_id, c_mask_attachment_id, c_mask_attachment_type)
    REFERENCES t_attachment (c_program_id, c_attachment_id, c_attachment_type)
    ON DELETE SET NULL (c_mask_attachment_id, c_mask_attachment_type)
);

COMMENT ON TABLE t_gmos_north_mos IS 'GMOS North MOS mode configuration';

CREATE TABLE t_gmos_south_mos (

  c_observation_id             d_observation_id      NOT NULL,
  c_program_id                 d_program_id          NOT NULL,
  c_instrument                 d_tag                 NOT NULL DEFAULT 'GmosSouth' REFERENCES t_instrument(c_tag) CHECK (c_instrument = 'GmosSouth'),
  c_observing_mode_type        e_observing_mode_type NOT NULL DEFAULT 'gmos_south_mos' CHECK (c_observing_mode_type = 'gmos_south_mos'),

  c_grating                    d_tag                 NOT NULL          REFERENCES t_gmos_south_disperser(c_tag),
  c_filter                     d_tag                 NULL DEFAULT NULL REFERENCES t_gmos_south_filter(c_tag),
  c_central_wavelength         d_wavelength_pm       NOT NULL,

  c_slit_width                 d_tag                 NOT NULL          REFERENCES t_gmos_custom_slit_width(c_tag),
  c_mask_attachment_id         d_attachment_id       NULL DEFAULT NULL,
  c_mask_attachment_type       e_attachment_type     NULL DEFAULT NULL CHECK (c_mask_attachment_type = 'mos_mask'),

  c_xbin                       d_tag                 NULL DEFAULT NULL REFERENCES t_gmos_binning(c_tag),
  c_xbin_default               d_tag                 NOT NULL DEFAULT 'One'::d_tag REFERENCES t_gmos_binning(c_tag),
  c_ybin                       d_tag                 NULL DEFAULT NULL REFERENCES t_gmos_binning(c_tag),
  c_ybin_default               d_tag                 NOT NULL DEFAULT 'One'::d_tag REFERENCES t_gmos_binning(c_tag),
  c_amp_read_mode              d_tag                 NULL DEFAULT NULL REFERENCES t_gmos_amp_read_mode(c_tag),
  c_amp_gain                   d_tag                 NULL DEFAULT NULL REFERENCES t_gmos_amp_gain(c_tag),
  c_roi                        d_tag                 NULL DEFAULT NULL REFERENCES t_gmos_roi(c_tag),

  c_wavelength_dithers         text                  NULL DEFAULT NULL,
  c_offsets                    text                  NULL DEFAULT NULL,

  c_initial_grating            d_tag                 NOT NULL          REFERENCES t_gmos_south_disperser(c_tag),
  c_initial_filter             d_tag                 NULL DEFAULT NULL REFERENCES t_gmos_south_filter(c_tag),
  c_initial_slit_width         d_tag                 NOT NULL          REFERENCES t_gmos_custom_slit_width(c_tag),
  c_initial_central_wavelength d_wavelength_pm       NOT NULL,

  CONSTRAINT gmos_south_mos_wavelength_dither_format CHECK (c_wavelength_dithers ~ '^-?\d+(\.\d+)?(,-?\d+(\.\d+)?)*$'),
  CONSTRAINT gmos_south_mos_offset_format            CHECK (c_offsets ~ '^-?\d+(\.\d+)?(,-?\d+(\.\d+)?)*$'),

  CONSTRAINT gmos_south_mos_mask_attachment_check
    CHECK ((c_mask_attachment_id IS NULL) = (c_mask_attachment_type IS NULL)),

  PRIMARY KEY (c_observation_id, c_instrument, c_observing_mode_type),
  UNIQUE (c_observation_id),
  FOREIGN KEY (c_observation_id, c_instrument, c_observing_mode_type)
    REFERENCES t_observation(c_observation_id, c_instrument, c_observing_mode_type)
    ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED,

  CONSTRAINT gmos_south_mos_mask_attachment_fkey
    FOREIGN KEY (c_program_id, c_mask_attachment_id, c_mask_attachment_type)
    REFERENCES t_attachment (c_program_id, c_attachment_id, c_attachment_type)
    ON DELETE SET NULL (c_mask_attachment_id, c_mask_attachment_type)
);

COMMENT ON TABLE t_gmos_south_mos IS 'GMOS South MOS mode configuration';

-- Default binning.  MOS uses the same spectral/spatial calculation as long slit
-- (cf. lucuma-core's gmos.mos.northBinning, which delegates to the same
-- functions with the same 2x spatial cap), reading the slit width from the
-- custom mask rather than from a builtin FPU.
CREATE OR REPLACE PROCEDURE set_gmos_default_binning(
  oid d_observation_id
) AS $$
DECLARE
  iq              numeric(2, 1);
  mode            e_observing_mode_type;
  pixel_scale_µas d_angle_µas;
  slit_µas        d_angle_µas;
  dispersion_pm   smallint;
  resolution      smallint;
  blaze_nm        smallint;
  src_profile     jsonb;
  profiles        jsonb[];
  current_xbin    smallint;
  current_ybin    smallint;
  min_xbin        smallint := 100;
  min_ybin        smallint := 100;
  xbin            d_tag;
  ybin            d_tag;
BEGIN

  -- Determine the IQ and observing mode.
  SELECT q.c_value, o.c_observing_mode_type
  INTO iq, mode
  FROM t_observation o
  LEFT JOIN t_image_quality q on o.c_image_quality = q.c_tag
  WHERE o.c_observation_id = oid;

  -- Lookup the slit width, dispersion, resolution and blaze wavelength.
  CASE
    WHEN mode = 'gmos_north_imaging' THEN
      SELECT c_pixel_size FROM t_gmos_north_detector WHERE c_tag = 'HAMAMATSU' INTO pixel_scale_µas;

    WHEN mode = 'gmos_north_long_slit' THEN
      SELECT c_pixel_size FROM t_gmos_north_detector WHERE c_tag = 'HAMAMATSU' INTO pixel_scale_µas;

      SELECT f.c_slit_width, d.c_dispersion_pm, d.c_reference_resolution, d.c_blaze_wavelength_nm
      INTO slit_µas, dispersion_pm, resolution, blaze_nm
      FROM t_gmos_north_long_slit g
      LEFT JOIN t_gmos_north_fpu       f ON g.c_fpu     = f.c_tag
      LEFT JOIN t_gmos_north_disperser d ON g.c_grating = d.c_tag
      WHERE g.c_observation_id = oid;

    WHEN mode = 'gmos_north_mos' THEN
      SELECT c_pixel_size FROM t_gmos_north_detector WHERE c_tag = 'HAMAMATSU' INTO pixel_scale_µas;

      SELECT w.c_width, d.c_dispersion_pm, d.c_reference_resolution, d.c_blaze_wavelength_nm
      INTO slit_µas, dispersion_pm, resolution, blaze_nm
      FROM t_gmos_north_mos g
      LEFT JOIN t_gmos_custom_slit_width w ON g.c_slit_width = w.c_tag
      LEFT JOIN t_gmos_north_disperser   d ON g.c_grating    = d.c_tag
      WHERE g.c_observation_id = oid;

    WHEN mode = 'gmos_south_imaging' THEN
      SELECT c_pixel_size FROM t_gmos_south_detector WHERE c_tag = 'HAMAMATSU' INTO pixel_scale_µas;

    WHEN mode = 'gmos_south_long_slit' THEN
      SELECT c_pixel_size FROM t_gmos_south_detector WHERE c_tag = 'HAMAMATSU' INTO pixel_scale_µas;

      SELECT f.c_slit_width, d.c_dispersion_pm, d.c_reference_resolution, d.c_blaze_wavelength_nm
      INTO slit_µas, dispersion_pm, resolution, blaze_nm
      FROM t_gmos_south_long_slit g
      LEFT JOIN t_gmos_south_fpu       f ON g.c_fpu     = f.c_tag
      LEFT JOIN t_gmos_south_disperser d ON g.c_grating = d.c_tag
      WHERE g.c_observation_id = oid;

    WHEN mode = 'gmos_south_mos' THEN
      SELECT c_pixel_size FROM t_gmos_south_detector WHERE c_tag = 'HAMAMATSU' INTO pixel_scale_µas;

      SELECT w.c_width, d.c_dispersion_pm, d.c_reference_resolution, d.c_blaze_wavelength_nm
      INTO slit_µas, dispersion_pm, resolution, blaze_nm
      FROM t_gmos_south_mos g
      LEFT JOIN t_gmos_custom_slit_width w ON g.c_slit_width = w.c_tag
      LEFT JOIN t_gmos_south_disperser   d ON g.c_grating    = d.c_tag
      WHERE g.c_observation_id = oid;

    ELSE
      RETURN;
  END CASE;

  -- Get all the source profiles in the asterism.
  SELECT ARRAY(
    SELECT t.c_source_profile
    FROM t_asterism_target a
    LEFT JOIN t_target t ON a.c_target_id = t.c_target_id
    WHERE a.c_observation_id = oid
  ) INTO profiles;

  -- HERE we want to compute the spectral (xbin) and spatial (ybin) binning for
  -- each profile and take the minimum value in each case.
  FOREACH src_profile IN ARRAY profiles
  LOOP
    -- Compute spectral (xbin) and spatial (ybin) binning for each profile
    CASE
      WHEN mode = 'gmos_north_imaging' OR mode = 'gmos_south_imaging' THEN
        current_xbin := calculate_gmos_spatial_binning(
          iq, pixel_scale_µas, src_profile, sampling := 2.5
        );

        -- Imaging binning capped at 2.
        IF current_xbin > 2 THEN
          current_xbin := 2;
        END IF;

        current_ybin := current_xbin;

      WHEN mode IN ('gmos_north_long_slit', 'gmos_south_long_slit', 'gmos_north_mos', 'gmos_south_mos') THEN
        current_xbin := calculate_gmos_spectral_binning(
          iq, slit_µas, dispersion_pm, resolution, blaze_nm, src_profile, sampling := 2.5
        );
        current_ybin := calculate_gmos_spatial_binning(
          iq, pixel_scale_µas, src_profile, sampling := 2.5
        );

        -- Spatial binning capped at 2.
        IF current_ybin > 2 THEN
          current_ybin := 2;
        END IF;

      ELSE
        RETURN;
    END CASE;

    -- Track minimum xbin and ybin values
    min_xbin := LEAST(COALESCE(min_xbin, current_xbin), current_xbin);
    min_ybin := LEAST(COALESCE(min_ybin, current_ybin), current_ybin);
  END LOOP;

  -- Turn the binning number into a d_tag, if possible.
  xbin := lookup_bin_tag(min_xbin);
  ybin := lookup_bin_tag(min_ybin);

  -- Set the binning in the appropriate mode table.
  IF xbin IS NOT NULL AND ybin IS NOT NULL THEN
    CASE
      WHEN mode = 'gmos_north_imaging' OR mode = 'gmos_south_imaging' THEN
        EXECUTE format('UPDATE t_%I SET c_bin_default = $1 WHERE c_observation_id = $2', mode::text)
        USING ybin, oid;

      WHEN mode IN ('gmos_north_long_slit', 'gmos_south_long_slit', 'gmos_north_mos', 'gmos_south_mos') THEN
        EXECUTE format('UPDATE t_%I SET c_xbin_default = $1, c_ybin_default = $2 WHERE c_observation_id = $3', mode::text)
        USING xbin, ybin, oid;

      ELSE
        RETURN;
    END CASE;
  END IF;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER gmos_north_mos_binning_trigger
AFTER INSERT OR DELETE OR UPDATE OF c_slit_width, c_grating
ON t_gmos_north_mos
FOR EACH ROW
EXECUTE FUNCTION trigger_set_gmos_default_binning();

CREATE TRIGGER gmos_south_mos_binning_trigger
AFTER INSERT OR DELETE OR UPDATE OF c_slit_width, c_grating
ON t_gmos_south_mos
FOR EACH ROW
EXECUTE FUNCTION trigger_set_gmos_default_binning();

-- Mode grouping.  MOS keys on the same fields as long slit; the custom slit
-- width takes the FPU slot and the observing mode type in the key keeps MOS and
-- long slit rows from colliding.
ALTER TABLE t_gmos_north_mos
  ADD COLUMN c_mode_key text NOT NULL GENERATED ALWAYS AS (
    format_gmos_long_slit_mode_group(
      'gn',
      c_program_id,
      c_observing_mode_type,
      c_grating,
      c_filter,
      c_slit_width,
      c_central_wavelength,
      c_xbin,
      c_xbin_default,
      c_ybin,
      c_ybin_default,
      c_amp_read_mode,
      c_amp_gain,
      c_roi,
      c_wavelength_dithers,
      c_offsets
    )
  ) STORED;

ALTER TABLE t_gmos_south_mos
  ADD COLUMN c_mode_key text NOT NULL GENERATED ALWAYS AS (
    format_gmos_long_slit_mode_group(
      'gs',
      c_program_id,
      c_observing_mode_type,
      c_grating,
      c_filter,
      c_slit_width,
      c_central_wavelength,
      c_xbin,
      c_xbin_default,
      c_ybin,
      c_ybin_default,
      c_amp_read_mode,
      c_amp_gain,
      c_roi,
      c_wavelength_dithers,
      c_offsets
    )
  ) STORED;

CREATE TRIGGER observing_mode_key_trigger
AFTER INSERT OR DELETE OR UPDATE OF c_mode_key ON t_gmos_north_mos
FOR EACH ROW
EXECUTE FUNCTION trigger_set_observation_mode_key();

CREATE TRIGGER observing_mode_key_trigger
AFTER INSERT OR DELETE OR UPDATE OF c_mode_key ON t_gmos_south_mos
FOR EACH ROW
EXECUTE FUNCTION trigger_set_observation_mode_key();

DROP VIEW v_observing_mode_group;
DROP VIEW v_all_modes;

CREATE VIEW v_all_modes AS
  SELECT c_mode_key, c_observation_id FROM t_flamingos_2_long_slit
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_ghost_ifu
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_gmos_north_long_slit
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_gmos_north_mos
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_gmos_south_long_slit
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_gmos_south_mos
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

-- The mode views.  Every default is already a stored column here, so these add
-- nothing today; they exist so the mappings read v_* like every other mode and
-- a computed column can be introduced later without touching them.
CREATE VIEW v_gmos_north_mos AS
  SELECT m.* FROM t_gmos_north_mos m;

CREATE VIEW v_gmos_south_mos AS
  SELECT m.* FROM t_gmos_south_mos m;

-- Register the modes for the observing mode consistency trigger.
SELECT register_observing_mode('gmos_north_mos', 't_gmos_north_mos');
SELECT register_observing_mode('gmos_south_mos', 't_gmos_south_mos');

-- Update check_etm_consistent to handle the MOS modes.  MOS has a science
-- exposure time mode but no acquisition, since it has no acquisition sequence.
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
      WHEN obs_mode IN ('flamingos_2_long_slit', 'gmos_north_long_slit', 'gmos_south_long_slit', 'gnirs_long_slit', 'gnirs_ifu') THEN
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

      WHEN obs_mode IN ('igrins_2_long_slit', 'gmos_north_mos', 'gmos_south_mos') THEN
        IF sci_count <> 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have exactly one science exposure time mode', obs_id, obs_mode;
        END IF;

      WHEN obs_mode IN ('gmos_north_imaging', 'gmos_south_imaging', 'flamingos_2_imaging', 'gnirs_imaging') THEN
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

-- Obs events and obs calc triggers
CREATE TRIGGER ch_observation_edit_gmos_north_mos_trigger
AFTER INSERT OR UPDATE OR DELETE ON t_gmos_north_mos
FOR EACH ROW
EXECUTE FUNCTION ch_observation_edit_associated_table_update();

CREATE TRIGGER ch_observation_edit_gmos_south_mos_trigger
AFTER INSERT OR UPDATE OR DELETE ON t_gmos_south_mos
FOR EACH ROW
EXECUTE FUNCTION ch_observation_edit_associated_table_update();

-- Configuration requests.  A MOS request is discriminated by its grating alone,
-- exactly as long slit, so the existing grating column is reused and its CHECK
-- widened to admit the MOS mode.
-- The two checks were declared anonymously in V0913 and reference a second
-- column, so Postgres named them positionally rather than after their column.
-- Names looked up in pg_constraint, as in V1086.
ALTER TABLE t_configuration_request
  DROP CONSTRAINT t_configuration_request_check,
  DROP CONSTRAINT t_configuration_request_check1;

ALTER TABLE t_configuration_request
  ADD CONSTRAINT t_configuration_request_c_gmos_north_grating_check
    CHECK ((c_gmos_north_longslit_grating IS NOT NULL) = (c_observing_mode_type IN ('gmos_north_long_slit'::e_observing_mode_type, 'gmos_north_mos'::e_observing_mode_type))),
  ADD CONSTRAINT t_configuration_request_c_gmos_south_grating_check
    CHECK ((c_gmos_south_longslit_grating IS NOT NULL) = (c_observing_mode_type IN ('gmos_south_long_slit'::e_observing_mode_type, 'gmos_south_mos'::e_observing_mode_type)));

DROP VIEW v_configuration_request;

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
    CASE WHEN cr.c_observing_mode_type = 'gmos_north_mos' THEN cr.c_configuration_request_id END AS c_gmos_north_mos_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_south_mos' THEN cr.c_configuration_request_id END AS c_gmos_south_mos_id,
    CASE WHEN cr.c_observing_mode_type = 'igrins_2_long_slit' THEN cr.c_configuration_request_id END AS c_igrins_2_longslit_id,
    CASE WHEN cr.c_gnirs_longslit_grating IS NOT NULL AND cr.c_gnirs_longslit_camera IS NOT NULL AND cr.c_gnirs_longslit_prism IS NOT NULL THEN cr.c_configuration_request_id END AS c_gnirs_longslit_id,
    CASE WHEN cr.c_gnirs_ifu_grating IS NOT NULL AND cr.c_gnirs_ifu_fpu IS NOT NULL THEN cr.c_configuration_request_id END AS c_gnirs_ifu_id,
    CASE WHEN cr.c_visitor_radius IS NOT NULL THEN cr.c_configuration_request_id END AS c_visitor_id,
    CASE WHEN cr.c_visitor_radius IS NOT NULL THEN cr.c_observing_mode_type END AS c_visitor_mode,
    CASE WHEN cr.c_region_ra_arc_type = 'partial' THEN cr.c_configuration_request_id END AS c_partial_ra_region_id,
    CASE WHEN cr.c_region_dec_arc_type = 'partial' THEN cr.c_configuration_request_id END AS c_partial_dec_region_id
  FROM t_configuration_request cr
  ;
