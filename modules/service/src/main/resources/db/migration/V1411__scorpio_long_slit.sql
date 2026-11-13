-- SCORPIO Long Slit observing mode.
--
-- SCORPIO has no grating or filter to choose: every channel is exposed at once
-- through fixed dichroics, so the slit is the only instrument choice.  Only the
-- visible arm (g, r, i, z) is supported for now.

-- Mode grouping.  The slit alone identifies a SCORPIO long slit configuration:
-- exposure settings and telescope offsets do not split a group.
CREATE FUNCTION format_scorpio_long_slit_mode_group(
  program_id          d_program_id,
  observing_mode_type e_observing_mode_type,
  fpu                 d_tag
) RETURNS text AS $$
BEGIN
  RETURN concat_ws(
    ':',
    program_id::text,
    observing_mode_type::text,
    fpu::text
  );
END;
$$ LANGUAGE plpgsql IMMUTABLE;

CREATE TABLE t_scorpio_long_slit (

  c_observation_id          d_observation_id      NOT NULL,
  c_program_id              d_program_id          NOT NULL,
  c_instrument              d_tag                 NOT NULL DEFAULT 'Scorpio' REFERENCES t_instrument(c_tag) CHECK (c_instrument = 'Scorpio'),
  c_observing_mode_type     e_observing_mode_type NOT NULL DEFAULT 'scorpio_long_slit' CHECK (c_observing_mode_type = 'scorpio_long_slit'),

  c_fpu                     d_tag                 NOT NULL REFERENCES t_scorpio_fpu(c_tag),

  -- One science exposure time mode per visible channel, as GHOST keeps one per
  -- camera.
  c_g_exposure_time_mode_id integer               NOT NULL REFERENCES t_exposure_time_mode(c_exposure_time_mode_id),
  c_r_exposure_time_mode_id integer               NOT NULL REFERENCES t_exposure_time_mode(c_exposure_time_mode_id),
  c_i_exposure_time_mode_id integer               NOT NULL REFERENCES t_exposure_time_mode(c_exposure_time_mode_id),
  c_z_exposure_time_mode_id integer               NOT NULL REFERENCES t_exposure_time_mode(c_exposure_time_mode_id),

  -- Explicit telescope configs (discriminant + JSON), NULL for the default.
  c_slit_offset_mode        d_tag                 NULL DEFAULT NULL REFERENCES t_slit_offset_mode(c_tag),
  c_telescope_configs       text                  NULL DEFAULT NULL,

  CONSTRAINT scorpio_long_slit_telescope_configs_check
    CHECK ((c_slit_offset_mode IS NULL) = (c_telescope_configs IS NULL)),

  c_mode_key                text                  NOT NULL GENERATED ALWAYS AS (
    format_scorpio_long_slit_mode_group(
      c_program_id,
      c_observing_mode_type,
      c_fpu
    )
  ) STORED,

  PRIMARY KEY (c_observation_id, c_instrument, c_observing_mode_type),
  UNIQUE (c_observation_id),
  FOREIGN KEY (c_observation_id, c_instrument, c_observing_mode_type)
    REFERENCES t_observation(c_observation_id, c_instrument, c_observing_mode_type)
    ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED
);

COMMENT ON TABLE t_scorpio_long_slit IS 'SCORPIO Long Slit mode configuration';

CREATE TRIGGER observing_mode_key_trigger
AFTER INSERT OR DELETE OR UPDATE OF c_mode_key ON t_scorpio_long_slit
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
  SELECT c_mode_key, c_observation_id FROM t_gnirs_spectroscopy
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_scorpio_long_slit;

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

-- Registers the mode and adds the consistency and obscalc invalidation triggers.
SELECT register_observing_mode('scorpio_long_slit', 't_scorpio_long_slit');

CREATE TRIGGER ch_observation_edit_scorpio_long_slit_trigger
AFTER INSERT OR UPDATE OR DELETE ON t_scorpio_long_slit
FOR EACH ROW
EXECUTE FUNCTION ch_observation_edit_associated_table_update();

-- SCORPIO long slit has no acquisition exposure time mode and one science mode
-- per visible channel.
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

      WHEN obs_mode = 'scorpio_long_slit' THEN
        IF sci_count <> 4 THEN
          RAISE EXCEPTION 'Observation % with mode % must have four science exposure time modes (g, r, i and z channels)', obs_id, obs_mode;
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

      WHEN obs_mode IN ('exchange_keck', 'exchange_subaru', 'daytime_calibration') THEN
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

-- The table holds only the explicit override; the view adds the `_default` and
-- `_effective` columns as the other long slit modes do.
CREATE VIEW v_scorpio_long_slit AS
SELECT
  m.*,
  d.c_slit_offset_mode_default,
  d.c_telescope_configs_default,
  COALESCE(m.c_slit_offset_mode,  d.c_slit_offset_mode_default)  AS c_slit_offset_mode_effective,
  COALESCE(m.c_telescope_configs, d.c_telescope_configs_default) AS c_telescope_configs_effective
FROM t_scorpio_long_slit m
CROSS JOIN LATERAL (
  SELECT
    'nod_along_slit'::varchar AS c_slit_offset_mode_default,
    -- ATTENTION: duplicated from lucuma-core scorpio.NodAlongSlitDefaultTelescopeConfigs
    -- (q = 0, -15, +15, 0 arcsec, all guided).  Keep in sync.
    '[{"q":{"microarcseconds":0},"guiding":"ENABLED"},{"q":{"microarcseconds":-15000000},"guiding":"ENABLED"},{"q":{"microarcseconds":15000000},"guiding":"ENABLED"},{"q":{"microarcseconds":0},"guiding":"ENABLED"}]'::text AS c_telescope_configs_default
) d;
