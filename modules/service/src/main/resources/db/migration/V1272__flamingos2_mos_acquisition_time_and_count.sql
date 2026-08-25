-- Flamingos 2 MOS acquisition switches to a Time & Count exposure time mode,
-- defaulting to 5 seconds with a count of 1.

-- Rewrite the acquisition ETMs V1265 created as signal-to-noise 10.  A row that
-- is already Time & Count keeps whatever the user set.
UPDATE t_exposure_time_mode e
   SET c_exposure_time_mode = 'time_and_count'::e_exp_time_mode,
       c_exposure_time      = '5 seconds'::interval,
       c_exposure_count     = 1,
       c_signal_to_noise    = NULL,
       c_is_explicit        = true
  FROM t_observation o
 WHERE e.c_observation_id = o.c_observation_id
   AND o.c_observing_mode_type = 'flamingos_2_mos'
   AND e.c_role = 'acquisition'
   AND e.c_exposure_time_mode <> 'time_and_count'::e_exp_time_mode;

-- F2 MOS now joins GMOS MOS in requiring a Time & Count acquisition ETM.
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

-- The acquisition exposure time changes, so every F2 MOS digest does too.
UPDATE t_obscalc c
   SET c_obscalc_state = 'pending'
  FROM t_observation o
 WHERE c.c_observation_id = o.c_observation_id
   AND o.c_observing_mode_type = 'flamingos_2_mos';
