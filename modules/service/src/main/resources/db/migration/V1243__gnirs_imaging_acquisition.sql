-- GNIRS imaging acquisition customization, mirroring t_gnirs_spectroscopy.
--
-- The acquisition type, filter, coadds and (for Faint) sky offset become explicit
-- overrides of the otherwise automatic choices. A NULL c_acq_type means the type is
-- determined by the ITC brightness classification; a NULL c_acq_filter means the
-- acquisition images through the first (wavelength-ordered) science filter.
--
-- As in t_gnirs_spectroscopy the sky offset lives in two µas columns that together form
-- a single Option[Offset], and it is meaningful only for the Faint type.
ALTER TABLE t_gnirs_imaging
  ADD COLUMN c_acq_type         e_gnirs_acquisition_type NULL DEFAULT NULL,
  ADD COLUMN c_acq_coadds       int4                     NOT NULL DEFAULT 1 CHECK (c_acq_coadds > 0),
  ADD COLUMN c_acq_filter       d_tag                    NULL DEFAULT NULL REFERENCES t_gnirs_filter(c_tag),
  ADD COLUMN c_acq_sky_offset_p d_angle_µas              NULL DEFAULT NULL,
  ADD COLUMN c_acq_sky_offset_q d_angle_µas              NULL DEFAULT NULL;

ALTER TABLE t_gnirs_imaging
  ADD CONSTRAINT c_acq_sky_offset_both_or_neither
  CHECK ((c_acq_sky_offset_p IS NULL) = (c_acq_sky_offset_q IS NULL));

-- `IS NOT DISTINCT FROM` treats a NULL c_acq_type as "not Faint", so automatic rows
-- must not carry a sky offset either.
ALTER TABLE t_gnirs_imaging
  ADD CONSTRAINT c_acq_sky_offset_faint_only
  CHECK ((c_acq_sky_offset_p IS NOT NULL) = (c_acq_type IS NOT DISTINCT FROM 'Faint'));

-- The view selects i.*, whose column list was fixed when the view was created, so it has
-- to be rebuilt to pick up the new columns. Recreated verbatim from V1204 otherwise.
DROP VIEW v_gnirs_imaging;

CREATE VIEW v_gnirs_imaging AS
  SELECT
    i.*,
    -- well depth default: mirrors GnirsWellDepth.forCamera
    -- ATTENTION: This logic is duplicated from lucuma-core GnirsWellDepth. Modify in sync.
    (CASE
      WHEN i.c_camera IN ('ShortBlue', 'LongBlue') THEN 'Shallow'
      WHEN i.c_camera IN ('ShortRed',  'LongRed')  THEN 'Deep'
    END)::e_gnirs_well_depth AS c_well_depth_default,
    f.c_filters,
    CASE WHEN i.c_variant = 'grouped'      THEN i.c_observation_id END AS c_grouped_observation_id,
    CASE WHEN i.c_variant = 'interleaved'  THEN i.c_observation_id END AS c_interleaved_observation_id,
    CASE WHEN i.c_variant = 'pre_imaging'  THEN i.c_observation_id END AS c_pre_imaging_observation_id
  FROM
    t_gnirs_imaging i
  LEFT JOIN (
    SELECT
      c_observation_id,
      array_remove(array_agg(c_filter ORDER BY c_filter), NULL) AS c_filters
    FROM t_gnirs_imaging_filter
    WHERE c_version = 'current'
    GROUP BY c_observation_id
  ) AS f USING (c_observation_id);

-- GNIRS imaging now has a user-visible acquisition exposure time mode, so it needs
-- exactly one acquisition etm row (the service has always inserted one, but nothing
-- enforced it). Backfill any observation that is missing one before adding the check.
INSERT INTO t_exposure_time_mode (
  c_observation_id,
  c_role,
  c_exposure_time_mode,
  c_signal_to_noise,
  c_signal_to_noise_at
)
SELECT
  i.c_observation_id,
  'acquisition',
  'signal_to_noise',
  10,
  sci.c_signal_to_noise_at
FROM t_gnirs_imaging i
CROSS JOIN LATERAL (
  SELECT e.c_signal_to_noise_at
    FROM t_gnirs_imaging_filter f
    JOIN t_exposure_time_mode e ON e.c_exposure_time_mode_id = f.c_exposure_time_mode_id
   WHERE f.c_observation_id = i.c_observation_id
     AND f.c_version = 'current'
     AND e.c_signal_to_noise_at IS NOT NULL
   ORDER BY f.c_filter
   LIMIT 1
) AS sci
WHERE NOT EXISTS (
  SELECT 1 FROM t_exposure_time_mode m
   WHERE m.c_observation_id = i.c_observation_id
     AND m.c_role = 'acquisition'
);

-- Update check_etm_consistent: gnirs_imaging has per-filter science etms (any number)
-- plus exactly one acquisition etm.
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
