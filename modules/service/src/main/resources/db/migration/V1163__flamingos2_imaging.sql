-- Flamingos2 Imaging observing mode, very much based on GMOS imaging.
CREATE TABLE t_flamingos_2_imaging (

  c_observation_id        d_observation_id      NOT NULL,
  c_program_id            d_program_id          NOT NULL,
  c_instrument            d_tag                 NOT NULL DEFAULT 'Flamingos2' REFERENCES t_instrument(c_tag) CHECK (c_instrument = 'Flamingos2'),
  c_observing_mode_type   e_observing_mode_type NOT NULL DEFAULT 'flamingos_2_imaging' CHECK (c_observing_mode_type = 'flamingos_2_imaging'),

  -- Explicit overrides
  c_read_mode             d_tag                 NULL DEFAULT NULL REFERENCES t_f2_read_mode(c_tag),
  c_read_mode_default     d_tag                 NOT NULL DEFAULT 'Faint' REFERENCES t_f2_read_mode(c_tag),
  c_reads                 d_tag                 NULL DEFAULT NULL REFERENCES t_f2_reads(c_tag),
  c_reads_default         d_tag                 NOT NULL REFERENCES t_f2_reads(c_tag),
  c_decker                d_tag                 NULL DEFAULT NULL REFERENCES t_f2_decker(c_tag),
  c_decker_default        d_tag                 NOT NULL DEFAULT 'Imaging' REFERENCES t_f2_decker(c_tag),
  c_readout_mode          d_tag                 NULL DEFAULT NULL REFERENCES t_f2_readout_mode(c_tag),
  c_readout_mode_default  d_tag                 NOT NULL DEFAULT 'Science' REFERENCES t_f2_readout_mode(c_tag),
  c_offsets               text                  NULL DEFAULT NULL,

  PRIMARY KEY (c_observation_id, c_instrument, c_observing_mode_type),
  UNIQUE (c_observation_id),
  FOREIGN KEY (c_observation_id, c_instrument, c_observing_mode_type)
    REFERENCES t_observation(c_observation_id, c_instrument, c_observing_mode_type)
    ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED
);

COMMENT ON TABLE t_flamingos_2_imaging IS 'Flamingos2 Imaging mode configuration';

-- Keep c_reads_default in sync with the read mode
CREATE TRIGGER maintain_f2_imaging_reads_default
BEFORE INSERT OR UPDATE OF c_read_mode, c_read_mode_default
ON t_flamingos_2_imaging
FOR EACH ROW EXECUTE FUNCTION update_f2_reads();

-- Table for the list of filters used in a Flamingos2 imaging observation with
-- an associated etm
-- Modeled after the GMOS imaging filter table.
CREATE TABLE t_flamingos_2_imaging_filter (
  c_observation_id        d_observation_id             NOT NULL,
  c_filter                d_tag                        NOT NULL REFERENCES t_f2_filter(c_tag),
  c_version               e_observing_mode_row_version NOT NULL DEFAULT 'current',
  c_exposure_time_mode_id integer                      NOT NULL,
  c_role                  e_exposure_time_mode_role    NOT NULL DEFAULT 'science' CHECK (c_role = 'science'),

  PRIMARY KEY (c_observation_id, c_filter, c_version),
  CONSTRAINT t_flamingos_2_imaging_filter_unique_exposure_time_mode_id
    UNIQUE (c_exposure_time_mode_id),
  FOREIGN KEY (c_observation_id)
    REFERENCES t_flamingos_2_imaging(c_observation_id) ON DELETE CASCADE,
  FOREIGN KEY (c_exposure_time_mode_id, c_role)
    REFERENCES t_exposure_time_mode(c_exposure_time_mode_id, c_role)
    ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED
);

CREATE VIEW v_flamingos_2_imaging AS
  SELECT
    i.*,
    f.c_filters
  FROM
    t_flamingos_2_imaging i
  LEFT JOIN (
    SELECT
      c_observation_id,
      array_remove(array_agg(c_filter ORDER BY c_filter), NULL) AS c_filters
    FROM t_flamingos_2_imaging_filter
    WHERE c_version = 'current'
    GROUP BY c_observation_id
  ) AS f USING (c_observation_id);

-- Register the mode for the observing mode consistency trigger.
SELECT register_observing_mode('flamingos_2_imaging', 't_flamingos_2_imaging');

-- Update check_etm_consistent to handle flamingos_2_imaging.
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
      WHEN obs_mode IN ('flamingos_2_long_slit', 'gmos_north_long_slit', 'gmos_south_long_slit', 'gnirs_long_slit') THEN
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

      ELSE
        RAISE EXCEPTION 'Unknown observing mode % for observation %', obs_mode, obs_id;
    END CASE;
  END IF;

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

-- Obs events and obs calc triggers
CREATE TRIGGER ch_observation_edit_flamingos_2_imaging_trigger
AFTER INSERT OR UPDATE OR DELETE ON t_flamingos_2_imaging
FOR EACH ROW
EXECUTE FUNCTION ch_observation_edit_associated_table_update();
