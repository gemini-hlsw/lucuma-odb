-- filter is actually required
ALTER TABLE t_flamingos_2_long_slit
  ALTER COLUMN c_filter SET NOT NULL,
  ALTER COLUMN c_initial_filter SET NOT NULL;

ALTER TABLE t_flamingos_2_long_slit
  ADD COLUMN c_decker_default d_tag NOT NULL REFERENCES t_f2_decker(c_tag) DEFAULT 'LongSlit';

ALTER TABLE t_flamingos_2_long_slit
  ADD COLUMN c_readout_mode_default d_tag NOT NULL REFERENCES t_f2_readout_mode(c_tag) DEFAULT 'Science';

ALTER TABLE t_flamingos_2_long_slit
  DROP COLUMN c_mode_key CASCADE;

CREATE OR REPLACE FUNCTION format_flamingos_2_long_slit_mode_group(
  program_id            d_program_id,
  observing_mode_type   e_observing_mode_type,
  disperser             d_tag,
  filter                d_tag,
  fpu                   d_tag,
  read_mode             d_tag,
  read_mode_default     d_tag,
  reads                 d_tag,
  reads_default         d_tag,
  decker                d_tag,
  decker_default        d_tag,
  readout_mode          d_tag,
  readout_mode_default  d_tag
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
    fpu::text,
    COALESCE(read_mode, read_mode_default)::text,
    COALESCE(reads, reads)::text,
    COALESCE(decker, decker_default)::text,
    COALESCE(readout_mode, readout_mode_default)::text
  );
END;
$$ LANGUAGE plpgsql IMMUTABLE;

ALTER TABLE t_flamingos_2_long_slit
  ADD COLUMN c_mode_key text NOT NULL GENERATED ALWAYS AS (
    format_flamingos_2_long_slit_mode_group(
      c_program_id,
      c_observing_mode_type,
      c_disperser,
      c_filter,
      c_fpu,
      c_read_mode,
      c_read_mode_default,
      c_reads,
      c_reads_default,
      c_decker,
      c_decker_default,
      c_readout_mode,
      c_readout_mode_default
    )
  ) STORED;

-- recreate flamingos_2_long_slit view of observing modes
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
