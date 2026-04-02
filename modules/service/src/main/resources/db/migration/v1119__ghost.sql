
CREATE OR REPLACE FUNCTION format_ghost_ifu_mode_group(
  program_id             d_program_id,
  observing_mode_type    e_observing_mode_type,
  resolution_mode        d_tag,
  red_binning            d_tag,
  red_binning_default    d_tag,
  red_read_mode          d_tag,
  red_read_mode_default  d_tag,
  blue_binning           d_tag,
  blue_binning_default   d_tag,
  blue_read_mode         d_tag,
  blue_read_mode_default d_tag,
  ifu1_fiber_agitator    e_ghost_fiber_agitator,
  ifu2_fiber_agitator    e_ghost_fiber_agitator
) RETURNS text AS $$
DECLARE
BEGIN
  -- Concat all the fields together into a text value.  Here we use the default
  -- value when an explicit override is not present.
  RETURN concat_ws(
    ':',
    program_id::text,
    observing_mode_type::text,
    resolution_mode::text,
    COALESCE(red_binning, red_binning_default)::text,
    COALESCE(red_read_mode, red_read_mode_default)::text,
    COALESCE(blue_binning, blue_binning_default)::text,
    COALESCE(blue_read_mode, blue_read_mode_default)::text,
    ifu1_fiber_agitator::text,
    ifu2_fiber_agitator::text
  );
END;
$$ LANGUAGE plpgsql IMMUTABLE;

CREATE TABLE t_ghost_ifu (
  c_observation_id          d_observation_id       NOT NULL,
  c_program_id              d_program_id           NOT NULL,
  c_instrument              d_tag                  NOT NULL DEFAULT 'Ghost' REFERENCES t_instrument(c_tag) CHECK (c_instrument = 'Ghost'),
  c_observing_mode_type     e_observing_mode_type  NOT NULL DEFAULT 'ghost_ifu' check (c_observing_mode_type = 'ghost_ifu'),

  c_resolution_mode         d_tag                  NOT NULL REFERENCES t_ghost_resolution_mode(c_tag),

  c_red_exposure_time_mode  integer                NOT NULL REFERENCES t_exposure_time_mode(c_exposure_time_mode_id),
  c_red_binning             d_tag                  NULL     REFERENCES t_ghost_binning(c_tag)   DEFAULT NULL ,
  c_red_binning_default     d_tag                  NOT NULL REFERENCES t_ghost_binning(c_tag)   DEFAULT 'one_by_one',
  c_red_read_mode           d_tag                  NULL     REFERENCES t_ghost_read_mode(c_tag) DEFAULT NULL,
  c_red_read_mode_default   d_tag                  NOT NULL REFERENCES t_ghost_read_mode(c_tag) DEFAULT 'medium',

  c_blue_exposure_time_mode integer                NOT NULL REFERENCES t_exposure_time_mode(c_exposure_time_mode_id),
  c_blue_binning            d_tag                  NULL     REFERENCES t_ghost_binning(c_tag)   DEFAULT NULL ,
  c_blue_binning_default    d_tag                  NOT NULL REFERENCES t_ghost_binning(c_tag)   DEFAULT 'one_by_one',
  c_blue_read_mode          d_tag                  NULL     REFERENCES t_ghost_read_mode(c_tag) DEFAULT NULL,
  c_blue_read_mode_default  d_tag                  NOT NULL REFERENCES t_ghost_read_mode(c_tag) DEFAULT 'slow',

  c_ifu1_fiber_agitator     e_ghost_fiber_agitator NULL     DEFAULT NULL,
  c_ifu2_fiber_agitator     e_ghost_fiber_agitator NULL     DEFAULT NULL,

  c_mode_key                text                  NOT NULL GENERATED ALWAYS AS (
    format_ghost_ifu_mode_group(
      c_program_id,
      c_observing_mode_type,
      c_resolution_mode,
      c_red_binning,
      c_red_binning_default,
      c_red_read_mode,
      c_red_read_mode_default,
      c_blue_binning,
      c_blue_binning_default,
      c_blue_read_mode,
      c_blue_read_mode_default,
      c_ifu1_fiber_agitator,
      c_ifu2_fiber_agitator
    )
  ) STORED,

  PRIMARY KEY (c_observation_id, c_instrument, c_observing_mode_type),
  FOREIGN KEY (c_observation_id, c_instrument, c_observing_mode_type) REFERENCES t_observation(c_observation_id, c_instrument, c_observing_mode_type) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED
);

-- Update v_observing_mode_group to include GHOST IFU
CREATE OR REPLACE VIEW v_all_modes AS
  SELECT c_mode_key, c_observation_id FROM t_flamingos_2_long_slit
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_ghost_ifu
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_gmos_north_long_slit
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_gmos_south_long_slit
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_igrins_2_long_slit;

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


-- Update check_etm_consistent to handle ghost_ifu
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

      WHEN obs_mode = 'igrins_2_long_slit' THEN
        IF sci_count <> 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have exactly one science exposure time mode', obs_id, obs_mode;
        END IF;

      WHEN obs_mode IN ('gmos_north_imaging', 'gmos_south_imaging') THEN
        NULL;

      ELSE
        RAISE EXCEPTION 'Unknown observing mode % for observation %', obs_mode, obs_id;
    END CASE;
  END IF;

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;