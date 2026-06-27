-- Unify GNIRS long slit + IFU into a single "spectroscopy" observing mode backed by one
-- table. The long slit and the IFU share all configuration except the FPU: the table now
-- carries c_fpu_slit / c_fpu_ifu (exactly one), and two observing-mode-type values
-- (gnirs_long_slit / gnirs_ifu) are backed by this one table, coupled to the FPU kind.

-- 1. Drop dependent views (recreated below).
DROP VIEW v_observing_mode_group;
DROP VIEW v_all_modes;
DROP VIEW v_gnirs_long_slit;

-- 2. Drop the mode-key propagation trigger + generated column (the column depends on the
--    FPU column we are about to rename).
DROP TRIGGER observing_mode_key_trigger ON t_gnirs_long_slit;
ALTER TABLE t_gnirs_long_slit DROP COLUMN c_mode_key;

-- 3. Rename the table.
ALTER TABLE t_gnirs_long_slit RENAME TO t_gnirs_spectroscopy;

-- 4. FPU columns: rename the existing slit columns, drop their NOT NULL, and add the IFU
--    enum columns. Exactly one of slit / ifu must be present (current and initial).
ALTER TABLE t_gnirs_spectroscopy RENAME COLUMN c_fpu         TO c_fpu_slit;
ALTER TABLE t_gnirs_spectroscopy RENAME COLUMN c_initial_fpu TO c_initial_fpu_slit;
ALTER TABLE t_gnirs_spectroscopy
  ALTER COLUMN c_fpu_slit         DROP NOT NULL,
  ALTER COLUMN c_initial_fpu_slit DROP NOT NULL,
  ADD COLUMN c_fpu_ifu         e_gnirs_fpu_ifu,
  ADD COLUMN c_initial_fpu_ifu e_gnirs_fpu_ifu,
  ADD CONSTRAINT t_gnirs_spectroscopy_fpu_check CHECK (
    (c_fpu_slit IS NOT NULL AND c_fpu_ifu IS     NULL) OR
    (c_fpu_slit IS     NULL AND c_fpu_ifu IS NOT NULL)
  ),
  ADD CONSTRAINT t_gnirs_spectroscopy_initial_fpu_check CHECK (
    (c_initial_fpu_slit IS NOT NULL AND c_initial_fpu_ifu IS     NULL) OR
    (c_initial_fpu_slit IS     NULL AND c_initial_fpu_ifu IS NOT NULL)
  );

-- 5. Observing-mode-type: drop the slit-only default + CHECK and couple the type to the
--    FPU kind (long slit <-> c_fpu_slit, ifu <-> c_fpu_ifu).
ALTER TABLE t_gnirs_spectroscopy ALTER COLUMN c_observing_mode_type DROP DEFAULT;
-- Drop the prior slit-only type CHECK (its auto-generated name is not known here).
DO $$
DECLARE r record;
BEGIN
  FOR r IN
    SELECT conname FROM pg_constraint
    WHERE conrelid = 't_gnirs_spectroscopy'::regclass
      AND contype = 'c'
      AND pg_get_constraintdef(oid) LIKE '%c_observing_mode_type%'
  LOOP
    EXECUTE format('ALTER TABLE t_gnirs_spectroscopy DROP CONSTRAINT %I', r.conname);
  END LOOP;
END $$;
ALTER TABLE t_gnirs_spectroscopy
  ADD CONSTRAINT t_gnirs_spectroscopy_mode_type_check CHECK (
    (c_observing_mode_type = 'gnirs_long_slit' AND c_fpu_slit IS NOT NULL) OR
    (c_observing_mode_type = 'gnirs_ifu'       AND c_fpu_ifu  IS NOT NULL)
  );

-- 6. Recreate the configuration-grouping mode key (FPU-kind agnostic) and its trigger.
ALTER TABLE t_gnirs_spectroscopy
  ADD COLUMN c_mode_key text GENERATED ALWAYS AS (
    -- COALESCE over slit (text domain) and the IFU enum mapped to text literals.
    -- An enum->text cast is not immutable, so the IFU value is mapped explicitly.
    c_initial_grating || '/' || c_initial_prism || '/' || COALESCE(
      c_fpu_slit,
      CASE c_fpu_ifu
        WHEN 'LowResolution'  THEN 'LowResolution'
        WHEN 'HighResolution' THEN 'HighResolution'
      END
    )
  ) STORED;

CREATE TRIGGER observing_mode_key_trigger
AFTER INSERT OR DELETE OR UPDATE OF c_mode_key ON t_gnirs_spectroscopy
FOR EACH ROW
EXECUTE FUNCTION trigger_set_observation_mode_key();

-- 7. Recreate the mode view. Identical to the prior v_gnirs_long_slit, with the decker
--    default extended for the IFU (mirrors lucuma-core GnirsDecker.forIfu).
CREATE VIEW v_gnirs_spectroscopy AS
  SELECT
    ls.*,
    -- effective central wavelength: COALESCE(override, initial)
    COALESCE(ls.c_central_wavelength, ls.c_initial_central_wavelength) AS c_central_wavelength_effective,
    -- effective grating/prism: COALESCE(explicit, initial)
    COALESCE(ls.c_grating, ls.c_initial_grating) AS c_grating_effective,
    COALESCE(ls.c_prism,   ls.c_initial_prism)   AS c_prism_effective,
    -- decker: default, effective
    -- ATTENTION: This logic is duplicated from lucuma-core GnirsDecker. Modify in sync.
    d.c_decker_default,
    COALESCE(ls.c_decker, d.c_decker_default) AS c_decker_effective,
    -- well depth: default, effective
    -- ATTENTION: This logic is duplicated from lucuma-core GnirsWellDepth. Modify in sync.
    d.c_well_depth_default,
    COALESCE(ls.c_well_depth, d.c_well_depth_default) AS c_well_depth_effective,
    -- slit offset mode + telescope configs: default, effective
    d.c_slit_offset_mode_default,
    d.c_telescope_configs_default,
    COALESCE(ls.c_slit_offset_mode, d.c_slit_offset_mode_default) AS c_slit_offset_mode_effective,
    COALESCE(ls.c_telescope_configs, d.c_telescope_configs_default) AS c_telescope_configs_effective
  FROM t_gnirs_spectroscopy ls
  CROSS JOIN LATERAL (
    SELECT
      -- slit offset mode default (always nod_along_slit for GNIRS)
      'nod_along_slit'::varchar AS c_slit_offset_mode_default,
      -- decker default: IFU -> forIfu(ifu); otherwise forCameraAndPrism(camera, effectivePrism)
      (CASE
        WHEN ls.c_fpu_ifu = 'LowResolution'  THEN 'LowResolutionIfu'
        WHEN ls.c_fpu_ifu = 'HighResolution' THEN 'HighResolutionIfu'
        WHEN COALESCE(ls.c_prism, ls.c_initial_prism) = 'Mirror' THEN
          CASE WHEN ls.c_camera IN ('ShortRed', 'ShortBlue') THEN 'ShortCamLongSlit'
               ELSE 'LongCamLongSlit'
          END
        ELSE -- Sxd or Lxd
          CASE WHEN ls.c_camera IN ('ShortRed', 'ShortBlue') THEN 'ShortCamCrossDispersed'
               ELSE 'LongCamCrossDispersed'
          END
      END)::e_gnirs_decker AS c_decker_default,
      -- well depth default: mirrors GnirsWellDepth.forCamera
      (CASE
        WHEN ls.c_camera IN ('ShortBlue', 'LongBlue') THEN 'Shallow'
        WHEN ls.c_camera IN ('ShortRed',  'LongRed')  THEN 'Deep'
      END)::e_gnirs_well_depth AS c_well_depth_default,
      -- telescope configs default:
      -- Cross-dispersed prisms → [-1", +2", +2", -1"] along slit
      -- Short camera long slit  → [+2", -4", -4", +2"] along slit
      -- Long camera, filter >= 2.5 µm (Order2/Order1/PAH) → [-3", +3", +3", -3"] along slit
      -- Long camera, filter < 2.5 µm (all others)         → [-1", +5", +5", -1"] along slit
      CASE
        WHEN COALESCE(ls.c_prism, ls.c_initial_prism) IN ('Sxd', 'Lxd') THEN
          '[{"q":{"microarcseconds":-1000000},"guiding":"ENABLED"},{"q":{"microarcseconds":2000000},"guiding":"ENABLED"},{"q":{"microarcseconds":2000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-1000000},"guiding":"ENABLED"}]'
        WHEN ls.c_camera IN ('ShortBlue', 'ShortRed') THEN
          '[{"q":{"microarcseconds":2000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-4000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-4000000},"guiding":"ENABLED"},{"q":{"microarcseconds":2000000},"guiding":"ENABLED"}]'
        WHEN ls.c_filter IN ('Order2', 'Order1', 'PAH') THEN
          '[{"q":{"microarcseconds":-3000000},"guiding":"ENABLED"},{"q":{"microarcseconds":3000000},"guiding":"ENABLED"},{"q":{"microarcseconds":3000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-3000000},"guiding":"ENABLED"}]'
        ELSE
          '[{"q":{"microarcseconds":-1000000},"guiding":"ENABLED"},{"q":{"microarcseconds":5000000},"guiding":"ENABLED"},{"q":{"microarcseconds":5000000},"guiding":"ENABLED"},{"q":{"microarcseconds":-1000000},"guiding":"ENABLED"}]'
      END AS c_telescope_configs_default
  ) d;

-- 8. Recreate v_all_modes (now referencing t_gnirs_spectroscopy) and v_observing_mode_group.
CREATE VIEW v_all_modes AS
  SELECT c_mode_key, c_observation_id FROM t_flamingos_2_long_slit
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_ghost_ifu
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_gmos_north_long_slit
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_gmos_south_long_slit
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_igrins_2_long_slit
  UNION ALL
  SELECT c_mode_key, c_observation_id FROM t_gnirs_spectroscopy;

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

-- 9. Registry: one table now backs two observing-mode types. Relax the unique table-name
--    constraint, repoint the existing long-slit row to the renamed table, and add the IFU
--    mapping. We do NOT call register_observing_mode for the IFU type: the per-table
--    consistency constraint trigger already exists on the (renamed) table and fires for
--    rows of either type.
-- Relax the unique table-name constraint (name not known here; drop any UNIQUE constraint).
DO $$
DECLARE r record;
BEGIN
  FOR r IN
    SELECT conname FROM pg_constraint
    WHERE conrelid = 't_observing_mode_registry'::regclass
      AND contype = 'u'
  LOOP
    EXECUTE format('ALTER TABLE t_observing_mode_registry DROP CONSTRAINT %I', r.conname);
  END LOOP;
END $$;
UPDATE t_observing_mode_registry
   SET c_table_name = 't_gnirs_spectroscopy'
 WHERE c_observing_mode_type = 'gnirs_long_slit';
INSERT INTO t_observing_mode_registry (c_observing_mode_type, c_table_name)
  VALUES ('gnirs_ifu', 't_gnirs_spectroscopy');

-- 10. ETM consistency: gnirs_ifu has the same requirement as the long slit (1 acquisition
--     + 1 science exposure time mode).
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

      ELSE
        RAISE EXCEPTION 'Unknown observing mode % for observation %', obs_mode, obs_id;
    END CASE;
  END IF;

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;
