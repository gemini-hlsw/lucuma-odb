-- Flamingos 2 MOS observing mode.
--
-- The mode is Flamingos 2 long slit with two substitutions: the builtin FPU is
-- replaced by a custom mask (a required slit width plus an optional attachment),
-- and the science telescope configs come from a MOS offset preset rather than
-- long slit's single fixed default.  The attachment is nullable because the mask
-- is usually only machined during Phase 2 (cf. V1230, which made the same
-- allowance for the dynamic config).

-- The MOS offset presets, following the t_slit_offset_mode pattern.
CREATE TABLE t_f2_mos_offset_preset (
  c_tag         d_tag PRIMARY KEY,
  c_description text NOT NULL
);

INSERT INTO t_f2_mos_offset_preset VALUES
  ('sparse_field',  'Sparse field'),
  ('crowded_field', 'Crowded field');

COMMENT ON TABLE t_f2_mos_offset_preset IS 'Flamingos 2 MOS offset presets';

CREATE TABLE t_flamingos_2_mos (

  c_observation_id       d_observation_id      NOT NULL,
  c_program_id           d_program_id          NOT NULL,
  c_instrument           d_tag                 NOT NULL DEFAULT 'Flamingos2' REFERENCES t_instrument(c_tag) CHECK (c_instrument = 'Flamingos2'),
  c_observing_mode_type  e_observing_mode_type NOT NULL DEFAULT 'flamingos_2_mos' CHECK (c_observing_mode_type = 'flamingos_2_mos'),

  c_disperser            d_tag                 NOT NULL          REFERENCES t_f2_disperser(c_tag),
  c_filter               d_tag                 NOT NULL          REFERENCES t_f2_filter(c_tag),

  -- The custom mask: the slit width is always known, the mask is usually only
  -- machined during Phase 2.  'Other' carries no width at all, so neither the
  -- ITC nor the equivalent builtin long slit FPU can be derived from it.
  c_slit_width           d_tag                 NOT NULL          REFERENCES t_f2_custom_slit_width(c_tag) CHECK (c_slit_width <> 'Other'),
  c_mask_attachment_id   d_attachment_id       NULL DEFAULT NULL,
  c_mask_attachment_type e_attachment_type     NULL DEFAULT NULL CHECK (c_mask_attachment_type = 'mos_mask'),

  -- Explicit overrides
  c_read_mode            d_tag                 NULL DEFAULT NULL REFERENCES t_f2_read_mode(c_tag),
  c_reads                d_tag                 NULL DEFAULT NULL REFERENCES t_f2_reads(c_tag),
  c_decker               d_tag                 NULL DEFAULT NULL REFERENCES t_f2_decker(c_tag),
  c_decker_default       d_tag                 NOT NULL DEFAULT 'MOS'     REFERENCES t_f2_decker(c_tag),
  c_readout_mode         d_tag                 NULL DEFAULT NULL REFERENCES t_f2_readout_mode(c_tag),
  c_readout_mode_default d_tag                 NOT NULL DEFAULT 'Science' REFERENCES t_f2_readout_mode(c_tag),

  c_mos_offset_preset    d_tag                 NOT NULL DEFAULT 'sparse_field' REFERENCES t_f2_mos_offset_preset(c_tag),
  c_slit_offset_mode     d_tag                 NULL DEFAULT NULL REFERENCES t_slit_offset_mode(c_tag),
  c_telescope_configs    text                  NULL DEFAULT NULL,

  c_telluric_type        jsonb                 NOT NULL DEFAULT '{"tag":"HOT","starTypes":null}'::jsonb,

  c_initial_disperser    d_tag                 NOT NULL          REFERENCES t_f2_disperser(c_tag),
  c_initial_filter       d_tag                 NOT NULL          REFERENCES t_f2_filter(c_tag),
  c_initial_slit_width   d_tag                 NOT NULL          REFERENCES t_f2_custom_slit_width(c_tag),

  -- Explicit configs are both-or-neither (a SlitTelescopeConfigs override needs both).
  CONSTRAINT flamingos_2_mos_explicit_configs_check
    CHECK ((c_telescope_configs IS NULL) = (c_slit_offset_mode IS NULL)),

  -- The type column exists only to pin the attachment's type through the
  -- composite foreign key, so it is present exactly when the id is.
  CONSTRAINT flamingos_2_mos_mask_attachment_check
    CHECK ((c_mask_attachment_id IS NULL) = (c_mask_attachment_type IS NULL)),

  PRIMARY KEY (c_observation_id, c_instrument, c_observing_mode_type),
  UNIQUE (c_observation_id),
  FOREIGN KEY (c_observation_id, c_instrument, c_observing_mode_type)
    REFERENCES t_observation(c_observation_id, c_instrument, c_observing_mode_type)
    ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED,

  -- The attachment if it exists must be a mask and on the same program.
  -- Deleting the mask returns the observation to "mask not yet defined", so the
  -- column-list form of SET NULL is used and the program id is left alone.
  CONSTRAINT flamingos_2_mos_mask_attachment_fkey
    FOREIGN KEY (c_program_id, c_mask_attachment_id, c_mask_attachment_type)
    REFERENCES t_attachment (c_program_id, c_attachment_id, c_attachment_type)
    ON DELETE SET NULL (c_mask_attachment_id, c_mask_attachment_type)
);

COMMENT ON TABLE t_flamingos_2_mos IS 'Flamingos 2 MOS mode configuration';

-- Mode grouping.  MOS keys on the same fields as long slit, with the custom slit
-- width taking the FPU slot and the observing mode type in the key.
ALTER TABLE t_flamingos_2_mos
  ADD COLUMN c_mode_key text NOT NULL GENERATED ALWAYS AS (
    format_flamingos_2_long_slit_mode_group(
      c_program_id,
      c_observing_mode_type,
      c_disperser,
      c_filter,
      c_slit_width,
      c_read_mode,
      c_reads,
      c_decker,
      c_decker_default,
      c_readout_mode,
      c_readout_mode_default
    )
  ) STORED;

CREATE TRIGGER observing_mode_key_trigger
AFTER INSERT OR DELETE OR UPDATE OF c_mode_key ON t_flamingos_2_mos
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

-- The mode view.  Unlike long slit, whose single default is a literal (V1219),
-- the defaults here select on the offset preset.
CREATE VIEW v_flamingos_2_mos AS
  SELECT
    m.*,
    d.c_slit_offset_mode_default,
    d.c_telescope_configs_default,
    COALESCE(m.c_slit_offset_mode,  d.c_slit_offset_mode_default)  AS c_slit_offset_mode_effective,
    COALESCE(m.c_telescope_configs, d.c_telescope_configs_default) AS c_telescope_configs_effective
  FROM t_flamingos_2_mos m
  CROSS JOIN LATERAL (
    SELECT
      -- Default slit offset mode (shape) per preset.
      CASE m.c_mos_offset_preset
        WHEN 'crowded_field' THEN 'nod_to_sky'
        ELSE                      'nod_along_slit'
      END::varchar AS c_slit_offset_mode_default,
      -- Default telescope configs JSON (transport codec).
      -- ATTENTION: duplicated from lucuma-core flamingos2.defaultMosTelescopeConfigs.
      -- Keep in sync.
      CASE m.c_mos_offset_preset
        WHEN 'crowded_field' THEN
          '[{"offset":{"p":{"microarcseconds":0},"q":{"microarcseconds":0}},"guiding":"ENABLED"},{"offset":{"p":{"microarcseconds":0},"q":{"microarcseconds":300000000}},"guiding":"DISABLED"},{"offset":{"p":{"microarcseconds":0},"q":{"microarcseconds":320000000}},"guiding":"DISABLED"},{"offset":{"p":{"microarcseconds":0},"q":{"microarcseconds":0}},"guiding":"ENABLED"}]'
        ELSE
          '[{"q":{"microarcseconds":1200000},"guiding":"ENABLED"},{"q":{"microarcseconds":-1200000},"guiding":"ENABLED"},{"q":{"microarcseconds":-1200000},"guiding":"ENABLED"},{"q":{"microarcseconds":1200000},"guiding":"ENABLED"}]'
      END::text AS c_telescope_configs_default
  ) d;

-- Register the mode for the observing mode consistency trigger.
SELECT register_observing_mode('flamingos_2_mos', 't_flamingos_2_mos');

-- Obs events and obs calc triggers
CREATE TRIGGER ch_observation_edit_flamingos_2_mos_trigger
AFTER INSERT OR UPDATE OR DELETE ON t_flamingos_2_mos
FOR EACH ROW
EXECUTE FUNCTION ch_observation_edit_associated_table_update();

-- F2 MOS has a science exposure time mode but no acquisition, since it has no
-- acquisition sequence.
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

      WHEN obs_mode IN ('gmos_north_mos', 'gmos_south_mos') THEN
        IF acq_count <> 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have an acquisition exposure time mode', obs_id, obs_mode;
        END IF;

        IF sci_count <> 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have exactly one science exposure time mode', obs_id, obs_mode;
        END IF;

        -- The acquisition mode must be Time & Count: there is no acquisition ITC
        -- pass to solve a signal-to-noise one.
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

      WHEN obs_mode IN ('flamingos_2_mos', 'igrins_2_long_slit') THEN
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

-- Configuration requests.  An F2 MOS request is discriminated by its disperser
-- alone, exactly as long slit, so the existing disperser column is reused and its
-- CHECK widened to admit the MOS mode.  The check was declared inline in V1031
-- and so carries a generated name; look it up rather than guess.
DO $$
DECLARE
  cname text;
BEGIN
  SELECT conname INTO cname
    FROM pg_constraint
   WHERE conrelid = 't_configuration_request'::regclass
     AND contype  = 'c'
     AND pg_get_constraintdef(oid) LIKE '%c_flamingos_2_longslit_disperser%';

  IF cname IS NOT NULL THEN
    EXECUTE format('ALTER TABLE t_configuration_request DROP CONSTRAINT %I', cname);
  END IF;
END $$;

ALTER TABLE t_configuration_request
  ADD CONSTRAINT t_configuration_request_c_flamingos_2_disperser_check
    CHECK ((c_flamingos_2_longslit_disperser IS NOT NULL) = (c_observing_mode_type IN ('flamingos_2_long_slit'::e_observing_mode_type, 'flamingos_2_mos'::e_observing_mode_type)));

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
    CASE WHEN cr.c_gnirs_longslit_grating IS NOT NULL AND cr.c_gnirs_longslit_camera IS NOT NULL AND cr.c_gnirs_longslit_prism IS NOT NULL THEN cr.c_configuration_request_id END AS c_gnirs_longslit_id,
    CASE WHEN cr.c_gnirs_ifu_grating IS NOT NULL AND cr.c_gnirs_ifu_fpu IS NOT NULL THEN cr.c_configuration_request_id END AS c_gnirs_ifu_id,
    CASE WHEN cr.c_visitor_radius IS NOT NULL THEN cr.c_configuration_request_id END AS c_visitor_id,
    CASE WHEN cr.c_visitor_radius IS NOT NULL THEN cr.c_observing_mode_type END AS c_visitor_mode,
    CASE WHEN cr.c_region_ra_arc_type = 'partial' THEN cr.c_configuration_request_id END AS c_partial_ra_region_id,
    CASE WHEN cr.c_region_dec_arc_type = 'partial' THEN cr.c_configuration_request_id END AS c_partial_dec_region_id
  FROM t_configuration_request cr
  ;
