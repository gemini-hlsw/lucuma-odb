-- Exchange observations: a Gemini PI configures an observation that requests
-- time at Keck or Subaru.  These instruments are not supported by the ITC or
-- AGS, so there is no exposure time mode and no generated sequence.  We capture
-- only the exchange instrument and the total requested observing time; the
-- observatory is encoded by the observing mode (exchange_keck / exchange_subaru).

-- Exchange instruments (Keck/Subaru) are not Gemini instruments, so they are
-- not registered in t_instrument and exchange observations leave c_instrument
-- null.  The specific exchange instrument is stored on t_exchange below.

-- Exchange config table.  Exactly the instrument column matching the observing
-- mode is set.
CREATE TABLE t_exchange (

  c_observation_id      d_observation_id      NOT NULL,
  c_keck_instrument     e_keck_instrument     NULL,
  c_subaru_instrument   e_subaru_instrument   NULL,
  c_total_request_time  interval              NOT NULL,

  -- exactly one instrument; the mode is derived from which one
  CHECK (num_nonnulls(c_keck_instrument, c_subaru_instrument) = 1),

  c_observing_mode_type e_observing_mode_type
    GENERATED ALWAYS AS (
      CASE WHEN c_keck_instrument IS NOT NULL
           THEN 'exchange_keck'::e_observing_mode_type
           ELSE 'exchange_subaru'::e_observing_mode_type
      END
    ) STORED,

  PRIMARY KEY (c_observation_id, c_observing_mode_type),
  FOREIGN KEY (c_observation_id, c_observing_mode_type)
    REFERENCES t_observation(c_observation_id, c_observing_mode_type)
    ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED
);

-- Exchange observations have no exposure time mode.
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

      -- exchange observations have no exposure time mode
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

SELECT register_observing_mode('exchange_keck',   't_exchange');
SELECT register_observing_mode('exchange_subaru', 't_exchange');

-- View exposing a non-null key for the (always present) total request time.
CREATE VIEW v_exchange AS
  SELECT
    e.*,
    CASE WHEN e.c_total_request_time IS NOT NULL THEN e.c_observation_id END
      AS c_total_request_time_id
  FROM t_exchange e;

-- Invalidate obscalc for exchange observations.
CREATE TRIGGER exchange_invalidate_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_exchange
  FOR EACH ROW EXECUTE FUNCTION obsid_obscalc_invalidate();
