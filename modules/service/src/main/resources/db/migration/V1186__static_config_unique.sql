-- Each execution static table is meant to hold exactly one row per observation.
-- V1117 deduped and dropped c_visit_id for some instrument and implicitly dropped
-- the UNIQUE (c_observation_id, c_visit_id) constraint)

-- Dedupe (keep the most recent row per observation) and add the missing
-- UNIQUE (c_observation_id) to every execution static table.
DO $$
DECLARE
  t_name  text;
  t_names text[] := array[
    't_gmos_north_static',
    't_gmos_south_static',
    't_flamingos_2_static',
    't_igrins_2_static',
    't_gnirs_static',
    't_ghost_static'
  ];
  deleted_count bigint;
BEGIN
  FOREACH t_name IN ARRAY t_names LOOP
    EXECUTE format($f$
      WITH ranked AS (
        SELECT
          c_static_id,
          ROW_NUMBER() OVER (
            PARTITION BY c_observation_id
            ORDER BY c_static_id DESC
          ) AS row_num
        FROM %I
      )
      DELETE FROM %I t
      USING ranked r
      WHERE t.c_static_id = r.c_static_id
        AND r.row_num > 1 $f$,
      t_name,
      t_name
    );

    GET DIAGNOSTICS deleted_count = ROW_COUNT;
    RAISE NOTICE 'Deleted % duplicate rows from %', deleted_count, t_name;

    EXECUTE format(
      'ALTER TABLE %I ADD CONSTRAINT %I UNIQUE (c_observation_id)',
      t_name,
      t_name || '_obs_unique'
    );
  END LOOP;
END $$;
