
-- Exposure time modes will be pulled into their own table.  There are n
-- exposure time modes per observation.  One from the science requirements, one
-- (potentially) for the acquisition, and n science exposure time modes (e.g.,
-- one per filter for imaging).  This enumeration identifies the possible roles
-- of an exposure time mode.
CREATE TYPE e_exposure_time_mode_role AS ENUM(
  'acquisition',
  'requirement',
  'science'
);


-- Exposure Time Mode gets its own table.
CREATE TABLE t_exposure_time_mode (
  c_exposure_time_mode_id SERIAL                    PRIMARY KEY,
  c_observation_id        d_observation_id          NOT NULL REFERENCES t_observation(c_observation_id) ON DELETE CASCADE,
  c_role                  e_exposure_time_mode_role NOT NULL,

  c_exposure_time_mode    e_exp_time_mode           NOT NULL,
  c_signal_to_noise       numeric(11, 3)            CHECK (c_signal_to_noise > 0::numeric),
  c_signal_to_noise_at    d_wavelength_pm,
  c_exposure_time         interval                  CHECK (c_exposure_time  >= '0'::interval),
  c_exposure_count        integer                   CHECK (c_exposure_count >= 0),

  CONSTRAINT t_exposure_time_mode_check CHECK (
    CASE c_exposure_time_mode
      WHEN 'signal_to_noise'::e_exp_time_mode THEN num_nulls(c_signal_to_noise, c_signal_to_noise_at) = 0
      WHEN 'time_and_count'::e_exp_time_mode  THEN num_nulls(c_signal_to_noise_at, c_exposure_time, c_exposure_count) = 0
      ELSE true
    END
  )
);

-- There may be only one requirement and one acquisition ETM per observations.
-- Some observing modes, however, will have multiple science ETMs
CREATE UNIQUE INDEX exposure_time_mode_role_constraint
  ON t_exposure_time_mode (c_observation_id, c_role)
  WHERE c_role IN (
    'requirement'::e_exposure_time_mode_role,
    'acquisition'::e_exposure_time_mode_role
  );

-- Populate the requirement ETMs from the existing requirements in t_observation
INSERT INTO t_exposure_time_mode (
  c_observation_id,
  c_role,
  c_exposure_time_mode,
  c_signal_to_noise,
  c_signal_to_noise_at,
  c_exposure_time,
  c_exposure_count
)
SELECT
  c_observation_id,
  'requirement'::e_exposure_time_mode_role,
  c_exp_time_mode,
  c_etm_signal_to_noise,
  c_etm_signal_to_noise_at,
  c_etm_exp_time,
  c_etm_exp_count
FROM t_observation
WHERE c_exp_time_mode IS NOT NULL;

DROP VIEW IF EXISTS v_observation;
DROP VIEW IF EXISTS v_generator_params;

-- Drop the now redundant columns in t_observation
ALTER TABLE t_observation
  DROP COLUMN c_exp_time_mode,
  DROP COLUMN c_etm_signal_to_noise,
  DROP COLUMN c_etm_signal_to_noise_at,
  DROP COLUMN c_etm_exp_time,
  DROP COLUMN c_etm_exp_count;

-- Migrate the existing long slit observing mode tables to have acquisition and
-- science ETMs based on a default (for acq) and the requirement ETM (for sci).
DO $$
DECLARE
  mode TEXT[];
  modes CONSTANT TEXT[][] := ARRAY[
    ['Flamingos2 Long Slit',
     't_flamingos_2_long_slit',
     'f.c_wavelength',
     'LEFT JOIN t_f2_filter f ON f.c_tag = m.c_filter'
    ],
    ['GMOS North Long Slit',
     't_gmos_north_long_slit',
     'm.c_central_wavelength',
     ''
    ],
    ['GMOS South Long Slit',
     't_gmos_south_long_slit',
     'm.c_central_wavelength',
     ''
    ]
  ];
BEGIN
  FOREACH mode SLICE 1 IN ARRAY modes
  LOOP
    RAISE NOTICE 'Converting mode %', mode[1];

    -- Copy requirement ETM into science ETM for every observation with this
    -- observing mode.
    EXECUTE format($sql$
      INSERT INTO t_exposure_time_mode (
        c_observation_id,
        c_role,
        c_exposure_time_mode,
        c_signal_to_noise,
        c_signal_to_noise_at,
        c_exposure_time,
        c_exposure_count
      )
      SELECT
        m.c_observation_id,
        'science'::e_exposure_time_mode_role,
        e.c_exposure_time_mode,
        e.c_signal_to_noise,
        e.c_signal_to_noise_at,
        e.c_exposure_time,
        e.c_exposure_count
      FROM %I m
      INNER JOIN t_exposure_time_mode e
        ON e.c_observation_id = m.c_observation_id
       AND e.c_role = 'requirement';
    $sql$, mode[2]);

    -- Remove observing mode entries without a corresponding science ETM.  That
    -- is, those that never had a requirement exposure time mode.  In order to
    -- actually execute an observation, a requirement exposure time mode must
    -- have been present. A science ETM is now going to be required.
    EXECUTE format($sql$
      DELETE FROM %I m
      WHERE NOT EXISTS (
        SELECT 1
        FROM t_exposure_time_mode e
        WHERE e.c_observation_id = m.c_observation_id
        AND e.c_role = 'science'
      );
    $sql$, mode[2]);

    -- Add an acquisition ETM for every remaining observing mode
    EXECUTE format($sql$
      INSERT INTO t_exposure_time_mode (
        c_observation_id,
        c_role,
        c_exposure_time_mode,
        c_signal_to_noise,
        c_signal_to_noise_at,
        c_exposure_time,
        c_exposure_count
      )
      SELECT
        c_observation_id,
        'acquisition'::e_exposure_time_mode_role,
        'signal_to_noise'::e_exp_time_mode,
        10::numeric(11,3),
        %s,
        NULL,
        NULL
      FROM %I m
      %s;
    $sql$, mode[3], mode[2], mode[4]);
  END LOOP;
END $$;

-- We'd like to add c_observing_mode_type and make a compound FK to
-- t_observation(c_observing_id, c_observing_mode_type) but unfortunately the
-- observing mode type is nullable.  We'll make sure the ETM is consistent with
-- the mode in a trigger function.

CREATE FUNCTION check_etm_consistent()
RETURNS TRIGGER AS $$
DECLARE
  obs_id   d_observation_id;
  obs_mode e_observing_mode_type;
  acq_count INTEGER;
  sci_count INTEGER;
BEGIN

  -- Use OLD for DELETE, NEW otherwise
  obs_id := COALESCE(NEW.c_observation_id, OLD.c_observation_id);

  -- Lookup the observing mode
  SELECT c_observing_mode_type INTO obs_mode
    FROM t_observation
   WHERE c_observation_id = obs_id;

  -- How many of each type do we have?
  SELECT
    COUNT(*) FILTER (WHERE c_role = 'acquisition'),
    COUNT(*) FILTER (WHERE c_role = 'science')
  INTO acq_count, sci_count
  FROM t_exposure_time_mode
  WHERE c_observation_id = obs_id;

  IF obs_mode IS NULL THEN

    -- When there is no observing mode, there should be no acquisition or science ETMs.
    IF acq_count <> 0 OR sci_count <> 0 THEN
      RAISE EXCEPTION 'Observation % with mode % should not have acquisition nor science exposure time modes', obs_id, obs_mode;
    END IF;

  ELSE

    -- Mode specific checks
    CASE
      WHEN obs_mode IN ('flamingos_2_long_slit', 'gmos_north_long_slit', 'gmos_south_long_slit') THEN
        IF acq_count <> 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have an acquisition exposure time mode', obs_id, obs_mode;
        END IF;

        IF sci_count <> 1 THEN
          RAISE EXCEPTION 'Observation % with mode % must have exactly one science exposure time mode', obs_id, obs_mode;
        END IF;

      WHEN obs_mode IN ('gmos_north_imaging', 'gmos_south_imaging') THEN
        NULL;  -- placeholder

      ELSE
        RAISE EXCEPTION 'Unknown observing mode % for observation %', obs_mode, obs_id;
    END CASE;
  END IF;

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER check_etm_consistent__t_exposure_time_mode_trigger
AFTER INSERT OR DELETE OR UPDATE OF c_role ON t_exposure_time_mode
DEFERRABLE INITIALLY DEFERRED
FOR EACH ROW
EXECUTE FUNCTION check_etm_consistent();

CREATE CONSTRAINT TRIGGER check_etm_consistent__t_observation_trigger
AFTER INSERT OR DELETE OR UPDATE OF c_observing_mode_type ON t_observation
DEFERRABLE INITIALLY DEFERRED
FOR EACH ROW
EXECUTE FUNCTION check_etm_consistent();

-- We need a view on the exposure time mode for the embedded s/n, t&c.
CREATE VIEW v_exposure_time_mode AS
  SELECT e.*,
  CASE WHEN e.c_exposure_time_mode = 'signal_to_noise' THEN e.c_exposure_time_mode_id END AS c_signal_to_noise_id,
  CASE WHEN e.c_exposure_time_mode = 'time_and_count'  THEN e.c_exposure_time_mode_id END AS c_time_and_count_id
FROM t_exposure_time_mode e;

-- Update the observation view to remove the etm id columns.
CREATE VIEW v_observation AS
  SELECT o.*,
  CASE WHEN o.c_explicit_ra              IS NOT NULL THEN o.c_observation_id END AS c_explicit_base_id,
  CASE WHEN o.c_air_mass_min             IS NOT NULL THEN o.c_observation_id END AS c_air_mass_id,
  CASE WHEN o.c_hour_angle_min           IS NOT NULL THEN o.c_observation_id END AS c_hour_angle_id,
  CASE WHEN o.c_observing_mode_type      IS NOT NULL THEN o.c_observation_id END AS c_observing_mode_id,
  CASE WHEN o.c_spec_wavelength          IS NOT NULL THEN o.c_observation_id END AS c_spec_wavelength_id,

  CASE WHEN o.c_spec_wavelength_coverage IS NOT NULL THEN o.c_observation_id END AS c_spec_wavelength_coverage_id,
  CASE WHEN o.c_spec_focal_plane_angle   IS NOT NULL THEN o.c_observation_id END AS c_spec_focal_plane_angle_id,
  CASE WHEN o.c_img_minimum_fov          IS NOT NULL THEN o.c_observation_id END AS c_img_minimum_fov_id,
  CASE WHEN o.c_observation_duration     IS NOT NULL THEN o.c_observation_id END AS c_observation_duration_id,
  CASE WHEN o.c_science_mode = 'imaging'::d_tag      THEN o.c_observation_id END AS c_imaging_mode_id,
  CASE WHEN o.c_science_mode = 'spectroscopy'::d_tag THEN o.c_observation_id END AS c_spectroscopy_mode_id,
  c.c_active_start::timestamp + (c.c_active_end::timestamp - c.c_active_start::timestamp) * 0.5 AS c_reference_time
  FROM t_observation o
  LEFT JOIN t_proposal p on p.c_program_id = o.c_program_id
  LEFT JOIN t_cfp c on p.c_cfp_id = c.c_cfp_id;

CREATE OR REPLACE VIEW v_generator_params AS
SELECT
  o.c_program_id,
  o.c_observation_id,
  o.c_calibration_role,
  o.c_image_quality,
  o.c_cloud_extinction,
  o.c_sky_background,
  o.c_water_vapor,
  o.c_air_mass_min,
  o.c_air_mass_max,
  o.c_hour_angle_min,
  o.c_hour_angle_max,
  e.c_exposure_time_mode,
  e.c_signal_to_noise,
  e.c_signal_to_noise_at,
  e.c_exposure_time,
  e.c_exposure_count,
  o.c_observing_mode_type,
  o.c_science_band,
  o.c_declared_complete,
  o.c_acq_reset_time,
  o.c_blind_offset_target_id,
  b.c_sid_rv AS c_blind_rv,
  b.c_source_profile AS c_blind_source_profile,
  t.c_target_id,
  t.c_sid_rv,
  t.c_source_profile
FROM
  t_observation o
LEFT JOIN t_target b ON c_target_id = o.c_blind_offset_target_id
LEFT JOIN LATERAL (
  SELECT t.c_target_id,
         t.c_sid_rv,
         t.c_source_profile
    FROM t_asterism_target a
    INNER JOIN t_target t
      ON a.c_target_id = t.c_target_id
     AND t.c_existence = 'present'
   WHERE a.c_observation_id = o.c_observation_id
) t ON TRUE
LEFT JOIN t_exposure_time_mode e
  ON e.c_observation_id = o.c_observation_id
 AND e.c_role = 'requirement'
ORDER BY
  o.c_observation_id,
  t.c_target_id;

-- Update the signal-to-noise type to match the exposure time mode
ALTER TABLE t_obscalc
  ALTER COLUMN c_img_single_sn  SET DATA TYPE numeric(11,3),
  ALTER COLUMN c_img_total_sn   SET DATA TYPE numeric(11,3),
  ALTER COLUMN c_spec_single_sn SET DATA TYPE numeric(11,3),
  ALTER COLUMN c_spec_total_sn  SET DATA TYPE numeric(11,3);

-- We need to trigger an obscalc update whenever an exposure time mode changes.
CREATE FUNCTION etm_obscalc_invalidate()
RETURNS TRIGGER AS $$
DECLARE
  obs_id d_observation_id;
BEGIN
  obs_id := COALESCE(NEW.c_observation_id, OLD.c_observation_id);

  IF ROW(NEW.*) IS DISTINCT FROM ROW(OLD.*) THEN
    CALL invalidate_obscalc(obs_id);
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER etm_obscalc_invalidate_trigger
AFTER INSERT OR UPDATE OR DELETE ON t_exposure_time_mode
DEFERRABLE INITIALLY DEFERRED
FOR EACH ROW
EXECUTE FUNCTION etm_obscalc_invalidate();