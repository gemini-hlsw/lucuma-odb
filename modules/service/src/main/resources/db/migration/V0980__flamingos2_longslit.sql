
CREATE OR REPLACE FUNCTION format_flamingos_2_long_slit_mode_group(
  site                  e_site,
  program_id            d_program_id,
  observing_mode_type   e_observing_mode_type,
  disperser             d_tag,
  filter                d_tag,
  fpu                   d_tag
) RETURNS text AS $$
DECLARE
BEGIN
  -- Concat all the fields together into a text value.  Here we use the default
  -- value when an explicit override is not present.
  RETURN concat_ws(
    ':',
    program_id::text,
    observing_mode_type::text,
    disperser::text,
    COALESCE(filter::text, 'None'),
    fpu::text
  );
END;
$$ LANGUAGE plpgsql IMMUTABLE;

--- FLAMINGOS 2 LONG SLIT OBSERVING MODE

CREATE TABLE t_flamingos_2_long_slit (

  c_observation_id             d_observation_id      NOT NULL,
  c_program_id                 d_program_id          NOT NULL,
  c_instrument                 d_tag NOT NULL DEFAULT 'Flamingos2' REFERENCES t_instrument(c_tag) CHECK (c_instrument = 'Flamingos2'),
  c_observing_mode_type        e_observing_mode_type NOT NULL DEFAULT 'flamingos_2_long_slit' check (c_observing_mode_type = 'flamingos_2_long_slit'),

  c_disperser                  d_tag                 NOT NULL             REFERENCES t_f2_disperser(c_tag),
  c_filter                     d_tag                 NULL DEFAULT NULL    REFERENCES t_f2_filter(c_tag),
  c_fpu                        d_tag                 NOT NULL             REFERENCES t_f2_fpu(c_tag),
  c_read_mode                  d_tag                 NULL DEFAULT NULL    REFERENCES t_f2_read_mode(c_tag),
  c_decker                     d_tag                 NULL DEFAULT NULL    REFERENCES t_f2_decker(c_tag),
  c_readout_mode               d_tag                 NULL DEFAULT NULL    REFERENCES t_f2_readout_mode(c_tag),
  c_reads                      d_tag                 NULL DEFAULT NULL    REFERENCES t_f2_reads(c_tag),
  c_window_cover               d_tag                 NULL DEFAULT NULL    REFERENCES t_f2_window_cover(c_tag),
  c_use_electronic_offsetting  BOOL                  NULL DEFAULT NULL,
  c_mode_key                   text                  NOT NULL GENERATED ALWAYS AS (
    format_flamingos_2_long_slit_mode_group(
     'gs',
      c_program_id,
      c_observing_mode_type,
      c_disperser,
      c_filter,
      c_fpu
    )
  ) STORED,

  PRIMARY KEY (c_observation_id, c_instrument, c_observing_mode_type),
  FOREIGN KEY (c_observation_id, c_instrument, c_observing_mode_type) REFERENCES t_observation(c_observation_id, c_instrument, c_observing_mode_type) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED
);

CREATE OR REPLACE VIEW v_flamingos_2_long_slit AS
SELECT
  m.*
FROM
  t_flamingos_2_long_slit m
INNER JOIN t_observation o
  ON m.c_observation_id = o.c_observation_id;

-- The observing mode group view contains an entry per distinct observing mode
-- and program combination across all observing modes.
CREATE OR REPLACE VIEW v_observing_mode_group AS
-- GMOS-N LongSlit
  SELECT
    m.c_mode_key,
    o.c_program_id,
    max(m.c_observation_id) as c_observation_id -- arbitrary, just pick one
  FROM
    t_gmos_north_long_slit m
  LEFT JOIN t_observation o ON m.c_observation_id = o.c_observation_id
  GROUP BY
    m.c_mode_key,
    o.c_program_id
-- GMOS-S LongSlit
UNION ALL
  SELECT
    m.c_mode_key,
    o.c_program_id,
    max(m.c_observation_id) as c_observation_id -- arbitrary, just pick one
  FROM
    t_gmos_south_long_slit m
  LEFT JOIN t_observation o ON m.c_observation_id = o.c_observation_id
  GROUP BY
    m.c_mode_key,
    o.c_program_id
-- F2 LongSlit
UNION ALL
  SELECT
    m.c_mode_key,
    o.c_program_id,
    max(m.c_observation_id) as c_observation_id -- arbitrary, just pick one
  FROM
    t_flamingos_2_long_slit m
  LEFT JOIN t_observation o ON m.c_observation_id = o.c_observation_id
  GROUP BY
    m.c_mode_key,
    o.c_program_id;
