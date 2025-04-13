
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
  c_read_mode_default          d_tag                 NOT NULL             REFERENCES t_f2_read_mode(c_tag) DEFAULT 'Faint',
  c_decker                     d_tag                 NULL DEFAULT NULL    REFERENCES t_f2_decker(c_tag),
  c_readout_mode               d_tag                 NULL DEFAULT NULL    REFERENCES t_f2_readout_mode(c_tag),
  c_reads                      d_tag                 NULL DEFAULT NULL    REFERENCES t_f2_reads(c_tag),
  c_reads_default              d_tag                 NOT NULL             REFERENCES t_f2_reads(c_tag),
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

-- update reads when read_mode changes
CREATE OR REPLACE FUNCTION update_f2_reads()
RETURNS TRIGGER AS $$
BEGIN
    -- Get the reads value from the reads_mode
    SELECT c_reads INTO NEW.c_reads_default
    FROM t_f2_read_mode
    -- Reads default depends on the read_mode, either explicit or calculated
    WHERE c_tag = COALESCE(NEW.c_read_mode, NEW.c_read_mode_default);

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER maintain_f2_reads_default
BEFORE INSERT OR UPDATE OF c_read_mode_default
ON t_flamingos_2_long_slit
FOR EACH ROW EXECUTE FUNCTION update_f2_reads();

-- View of flamingos_2_long_slit observing modes
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

-- Replace the observation view to add f2 long slit mode columns.
DROP VIEW v_observation;

CREATE OR REPLACE VIEW v_observation AS
  SELECT o.*,
  CASE WHEN o.c_explicit_ra              IS NOT NULL THEN o.c_observation_id END AS c_explicit_base_id,
  CASE WHEN o.c_air_mass_min             IS NOT NULL THEN o.c_observation_id END AS c_air_mass_id,
  CASE WHEN o.c_hour_angle_min           IS NOT NULL THEN o.c_observation_id END AS c_hour_angle_id,
  CASE WHEN o.c_observing_mode_type      IS NOT NULL THEN o.c_observation_id END AS c_observing_mode_id,
  CASE WHEN o.c_spec_wavelength          IS NOT NULL THEN o.c_observation_id END AS c_spec_wavelength_id,

  CASE WHEN o.c_spec_exp_time_mode       IS NOT NULL THEN o.c_observation_id END AS c_spec_exp_time_mode_id,
  CASE WHEN o.c_spec_exp_time_mode = 'signal_to_noise' THEN o.c_observation_id END AS c_spec_signal_to_noise_id,
  CASE WHEN o.c_spec_exp_time_mode = 'time_and_count'  THEN o.c_observation_id END AS c_spec_time_and_count_id,

  CASE WHEN o.c_spec_wavelength_coverage IS NOT NULL THEN o.c_observation_id END AS c_spec_wavelength_coverage_id,
  CASE WHEN o.c_spec_focal_plane_angle   IS NOT NULL THEN o.c_observation_id END AS c_spec_focal_plane_angle_id,
  CASE WHEN o.c_observation_duration     IS NOT NULL THEN o.c_observation_id END AS c_observation_duration_id,
  c.c_active_start::timestamp + (c.c_active_end::timestamp - c.c_active_start::timestamp) * 0.5 AS c_reference_time
  FROM t_observation o
  LEFT JOIN t_proposal p on p.c_program_id = o.c_program_id
  LEFT JOIN t_cfp c on p.c_cfp_id = c.c_cfp_id
  LEFT JOIN t_gmos_north_long_slit  mode_gnls ON o.c_observation_id = mode_gnls.c_observation_id
  LEFT JOIN t_gmos_south_long_slit  mode_gsls ON o.c_observation_id = mode_gsls.c_observation_id
  LEFT JOIN t_flamingos_2_long_slit mode_f2ls ON o.c_observation_id = mode_f2ls.c_observation_id;

